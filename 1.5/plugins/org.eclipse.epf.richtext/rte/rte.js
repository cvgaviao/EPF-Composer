//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//------------------------------------------------------------------------------
// @author Kelvin Low
// @since 1.0
//------------------------------------------------------------------------------
// Note: Mozilla/Firefox does not allow unprivileged scripts to invoke the cut,
// copy and paste commands. The Javascript must either be signed
// (see http://www.mozilla.org/projects/security/components/signed-scripts.html),
// or the users must change their preferences
// (see http://www.mozilla.org/editor/midasdemo/securityprefs.html).
// Alternatively, the users can use the ctrl-x, ctrl-c and ctrl-v keys.
//------------------------------------------------------------------------------

var rte = new RTE();

function RTE() {
}

RTE.prototype.STATUS_NOP = 0;
RTE.prototype.STATUS_INITIALIZED = 1;
RTE.prototype.STATUS_MODIFIED = 2;
RTE.prototype.STATUS_GET_TEXT = 3;
RTE.prototype.STATUS_KEY_DOWN = 4;
RTE.prototype.STATUS_KEY_UP = 5;
RTE.prototype.STATUS_SELECT_TEXT = 6;
RTE.prototype.STATUS_SELECT_CONTROL = 7;
RTE.prototype.STATUS_SELECT_NONE = 8;
RTE.prototype.STATUS_EXEC_CMD = 9;
RTE.prototype.STATUS_REFORMAT_LINKS = 10;

RTE.prototype.KEY_ARROW_DOWN = 40;
RTE.prototype.KEY_ARROW_LEFT = 37;
RTE.prototype.KEY_ARROW_RIGHT = 39;
RTE.prototype.KEY_ARROW_UP = 38;
RTE.prototype.KEY_BACKSPACE = 8;
RTE.prototype.KEY_END = 35;
RTE.prototype.KEY_HOME = 36;
RTE.prototype.KEY_PAGE_DOWN = 34;
RTE.prototype.KEY_PAGE_UP = 33;
RTE.prototype.KEY_TAB = 9;
RTE.prototype.KEY_C = 67;
RTE.prototype.KEY_F = 70;
RTE.prototype.KEY_S = 83;
RTE.prototype.KEY_V = 86;
RTE.prototype.KEY_X = 88;
RTE.prototype.KEY_Z = 90;

RTE.prototype.CMD_COPY = "copy";
RTE.prototype.CMD_CUT = "cut";
RTE.prototype.CMD_FIND_TEXT = "findText";
RTE.prototype.CMD_PASTE = "paste";
RTE.prototype.CMD_SAVE = "save";
RTE.prototype.CMD_SAVE_ALL = "saveAll";

RTE.prototype.TABLE_HEADERS_NONE = 0;
RTE.prototype.TABLE_HEADERS_COLS = 1;
RTE.prototype.TABLE_HEADERS_ROWS = 2;
RTE.prototype.TABLE_HEADERS_BOTH = 3;

RTE.prototype.STYLE_BOLD = 1;
RTE.prototype.STYLE_ITALIC = RTE.STYLE_BOLD << 1;
RTE.prototype.STYLE_UNDERLINE = RTE.STYLE_ITALIC << 1;
RTE.prototype.STYLE_SUBSCRIPT = RTE.STYLE_UNDERLINE << 1;
RTE.prototype.STYLE_SUPERSCRIPT = RTE.STYLE_SUBSCRIPT << 1;

RTE.prototype.editorId;
RTE.prototype.editorCSS;
RTE.prototype.baseHREF;
RTE.prototype.supportRichTextEditing = true;
RTE.prototype.editorDoc;
RTE.prototype.selection;
RTE.prototype.selectionRange;
RTE.prototype.readOnly = false;
RTE.prototype.initialized = false;
RTE.prototype.modified = false;
RTE.prototype.selectionInfo = null;


//Initializes the editor.
RTE.prototype.initEditor = function(id, css, baseURL) {
	this.editorId = id;
	this.editorCSS = css;
	this.baseHREF = baseURL;
	try {
		this.enableRichTextEditing('');
		this.initialized = true;
		this.setStatus(this.STATUS_INITIALIZED, null);
	}
	catch (e) {
		this.supportRichTextEditing = false;
	}
}

// Handles the key events.
RTE.prototype.keyPressed = function(event) {
	var keyCode = event.keyCode;
	if (keyCode == 0 && !document.all) {
		keyCode = event.charCode;
		switch (keyCode) {
			case 99:
				keyCode = rte.KEY_C;
				break;
			case 102:
				keyCode = rte.KEY_F;
				break;
			case 115:
				keyCode = rte.KEY_S;
				break;
			case 118:
				keyCode = rte.KEY_V;
				break;
			case 120:
				keyCode = rte.KEY_X;
				break;
			case 122:
				keyCode = rte.KEY_Z;
				break;
		}
	}
	var ctrlKey = event.ctrlKey;
	var shiftKey = event.shiftKey;
	
	switch(keyCode) {
		case rte.KEY_ARROW_DOWN:
		case rte.KEY_ARROW_LEFT:
		case rte.KEY_ARROW_RIGHT:
		case rte.KEY_ARROW_UP:
		case rte.KEY_END:
		case rte.KEY_HOME:
		case rte.KEY_PAGE_DOWN:
		case rte.KEY_PAGE_UP:
		case rte.KEY_TAB:
			break;
		case rte.KEY_BACKSPACE:
			if (!rte.readOnly) {
				setTimeout("rte.setStatus(rte.STATUS_MODIFIED, null);", 10);
			}
			break;
		case rte.KEY_C:
			if (ctrlKey) {
				rte.setStatus(rte.STATUS_KEY_DOWN, rte.CMD_COPY);
			}
			else if (!document.all && rte.readOnly) {
				event.preventDefault();
			}
			break;			
		case rte.KEY_F:
			if (ctrlKey) {
				if (document.all) {
					event.keyCode = -1;
					event.returnValue = false;
				}
				else {
					event.preventDefault();
				}
				rte.setStatus(rte.STATUS_KEY_DOWN, rte.CMD_FIND_TEXT);
			}
			else if (!document.all && rte.readOnly) {
				event.preventDefault();
			}
			break;
		case rte.KEY_S:
			if (!rte.readOnly && ctrlKey) {
				if (document.all) {
					event.keyCode = -1;
					event.returnValue = false;
				}
				else {
					event.preventDefault();
				}
				if (shiftKey) {
					rte.setStatus(rte.STATUS_KEY_DOWN, rte.CMD_SAVE_ALL);
				}
				else {
					rte.setStatus(rte.STATUS_KEY_DOWN, rte.CMD_SAVE);
				}
			}
			else if (!document.all && rte.readOnly) {
				event.preventDefault();
			}			
			break;
		case rte.KEY_V:
			if (ctrlKey) {		
				if (document.all) {
					event.keyCode = -1;
					event.returnValue = false;
					if (!rte.readOnly) {
						rte.setStatus(rte.STATUS_KEY_DOWN, rte.CMD_PASTE);
					}
				}
				else {
					if (!rte.readOnly) {
						// Workaround Mozilla/Firefox paste issues.
						setTimeout("rte.setStatus(rte.STATUS_KEY_DOWN, rte.CMD_PASTE);", 10);
					}
					else {
						event.preventDefault();
					}
				}
			}
			else if (!document.all && rte.readOnly) {
				event.preventDefault();
			}
			break;
		case rte.KEY_X:
			if (ctrlKey) {
				rte.setStatus(rte.STATUS_KEY_DOWN, rte.CMD_CUT);
			}
			else if (!document.all && rte.readOnly) {
				event.preventDefault();
			}
			break;
		case rte.KEY_Z:
			if (!rte.readOnly && ctrlKey) {
				setTimeout("rte.setStatus(rte.STATUS_MODIFIED, null);", 10);
			}
			else if (!document.all && rte.readOnly) {
				event.preventDefault();
			}			
			break;
		default:
			if (!document.all && rte.readOnly) {
				event.preventDefault();
			}
	}
}

RTE.prototype.selChanged = function(event) {
	rte.updateSelection();
}

RTE.prototype.enableRichTextEditing = function(html) {
	var doc = document.getElementById(this.editorId).contentWindow.document;
	doc.designMode = "on";
	
	var htmlSrc = '<html><head><title></title>';
	
	if (this.editorCSS != null && this.editorCSS != '') {
		htmlSrc += '<link rel="StyleSheet" href="' + this.editorCSS + '" type="text/css"/>';
	}
	
	if (this.baseHREF != null && this.baseHREF != '') {	
		htmlSrc += '<base href="' + this.baseHREF + '"/>';
	}
	
	if (!document.all && html == '') {
		// Mozilla/Firefox will only display the caret if <br/> is added to the HTML body.
		// Adding <br/> also enables the backspace and delete key by default. Otherwise, the
		// user need to enter some text before these 2 keys start to function.
		html = "<br />";
	}
	
	htmlSrc += '</head><body>' + html + '</body></html>';
	
	doc.open();
	doc.write(htmlSrc);
	doc.close();
	
	this.modified = false;

	if ("attachEvent" in doc) {
		doc.attachEvent("onkeydown", this.keyPressed);
		doc.attachEvent("onselectionchange", this.selChanged);
		// for DnD (internal)
		doc.body.attachEvent("ondrop", this.checkModified);
		// for image/table resizing:
		doc.body.attachEvent("onresizeend", this.checkModified);
	}	
	if ("addEventListener" in doc) {
		doc.addEventListener("keypress", this.keyPressed, true);
		doc.addEventListener("keypress", this.selChanged, false);
		doc.addEventListener("mouseup", this.selChanged, false);
		doc.addEventListener("dragdrop", this.checkModified, false);
		
		// check mouseup event for image/table resizing
		doc.addEventListener("mouseup", this.checkModified, false);
	}

	this.setStatus(this.STATUS_EXEC_CMD, 1);
}

// this one is for modification check on drag n drop within the RTE
// checkModified listener
RTE.prototype.checkModified = function(event) {
	setTimeout("rte.setStatus(rte.STATUS_MODIFIED, null);", 10);
}

// Sets the height of the editor.
RTE.prototype.setHeight = function(height) {
	if (this.initialized) {
		document.getElementById(this.editorId).height = height + "px";
	}
}

// Sets the status.
// Note: By default, Firefox disables changes to the status bar. For this to work, the user
// must set the global preference "dom.disable_window_status_change" to false.
// For Firefox 1.0.x, this setting can be made in /usr/firefox-1.0.7/defaults/pref/firefox.js.
RTE.prototype.setStatus = function(type, value) {
	var status = '$$$' + type;
	if (value != null && value != '') {
		status += ('$' + value);		
	}
	window.status = status;
	window.status = '$$$' + this.STATUS_NOP;
}

// Returns the HTML source.
RTE.prototype.getHTML = function() {
	var html = document.getElementById(this.editorId).contentWindow.document.body.innerHTML;
	if (html == "<P>&nbsp;</P>") {
		html = "";
	}
	if (html != null && html != '') {
		var regEx = new RegExp("\"file\:([^=]*)(/resources/)([^\"]+)\"", "g");
		html = html.replace(regEx, "\"./resources/$3\"");
		regEx = new RegExp("\"file\:([^=]*)/#([^\"]+)\"", "g");
		html = html.replace(regEx, "\"#$2\"");
	}
	return html;
}

//Returns the HTML source to the Java layer
RTE.prototype.getText = function() {
	var html = this.getHTML();
	this.setStatus(this.STATUS_GET_TEXT, html);
	return html;
}

RTE.prototype.setInnerHTML = function(html) {
	if (document.all) {
		// IE has problem setting complex HTML set via doc.body.innerHTML.
		this.enableRichTextEditing(html);
	}
	else {
		if (html == '') {
			// Mozilla/Firefox will only display the caret if <br/> is added to the HTML body.
			html = "<br/>";
		}
		var doc = document.getElementById(this.editorId).contentWindow.document;
		if (doc.body != null) {
			doc.body.innerHTML = html;
		}
		else {
			// Mozilla/Firefox can take a while to initialize document.body
			// after document.write().
			try {
				setTimeout("this.setInnerHTML('" + html + "');", 10);
			}
			catch (e) {
			}
		}
	}
}

// Sets the HTML source.
RTE.prototype.setText = function(html) {
	if (this.supportRichTextEditing) {
		html = this.decodeString(html);
		this.selectionInfo = getSelectionInfo();
		this.setInnerHTML(html);
		if (selectionInfo != null) {
			setTimeout("this.setSelection(this.selectionInfo);", 10);
		}
		this.modified = false;
		this.setStatus(this.STATUS_EXEC_CMD, 1);
	}
}

RTE.prototype.setSelection = function (selectionInfo) {
	if (!supportRichTextEditing) {
		return;
	}
	
	contentWindow = document.getElementById(editorId).contentWindow;
	editorDoc = contentWindow.document;
	
	try {
		if (document.all) {
			var startOffset = selectionInfo.start;
			var len = selectionInfo.len;
			if (startOffset == 0 && len == 0) {
				return;
			}
			var tempRange = editorDoc.body.createTextRange();
			tempRange.moveStart('character', startOffset);
			tempRange.collapse();
			tempRange.moveEnd('character', len);
			tempRange.select();
			tempRange.scrollIntoView();
		} else {
			selection = this.window.getSelection();
			var startContainer = selectionInfo.startContainer;
			var start = selectionInfo.start;
			var endContainer = selectionInfo.endContainer;
			var end = selectionInfo.end;
			var tempRange = document.createRange();
			tempRange.setStart(startContainer, start);
			tempRange.setEnd(endContainer, end);
			selection.removeAllRanges();
			selection.addRange(tempRange);
			contentWindow.focus();
		}
	} catch (e) {
	}
}

RTE.prototype.getSelectionInfo = function () {
	if (!supportRichTextEditing) {
		return null;
	}	
	
	contentWindow = document.getElementById(editorId).contentWindow;
	editorDoc = contentWindow.document;
	
	var tempSelRange;
	try {
	    if (document.all) {
			selection = editorDoc.selection;
			if (selection != null) {
				tempSelRange = selection.createRange();
			}
			// length of selection
			var tempSelLen = tempSelRange.text.length;
			// create new range
			var tempRange = editorDoc.body.createTextRange();
			// set end of new range to start of selection
			// this will throw an exception if tempSelRange is not in editor.doc.body (ie, at the start of the RTE).
			tempRange.setEndPoint("EndToStart", tempSelRange);
			// length of new range is the start offset
			var tempText = tempRange.text;
			// IE counts newlines as 2 characters for length property, but they count as 1 when using moveStart so remove the \r to make the count the same
			tempText = tempText.replace(/\r/g, "");
			var startOffset = tempText.length;
			
			return {start:startOffset, len:tempSelLen};
	    } else {
			selection = contentWindow.getSelection();
			if (selection != null) {
				tempSelRange = selection.getRangeAt(selection.rangeCount - 1).cloneRange();
			}
			return {startContainer: tempSelRange.startContainer, start:tempSelRange.startOffset, 
				endContainer: tempSelRange.endContainer, end:tempSelRange.endOffset};
	    }
	} catch (e) {
		return null;
	}
}
// Decodes the HTML passed from the Java layer.
RTE.prototype.decodeString = function(str) {
	if (str != null && str != '') {
		if (document.all) {
			str = str.replace(/%sq%/g, "'");
			str = str.replace(/%EOL%/g, "\n");
		}
		else {
			str = str.replace(/%sq%/g, "&apos;");
			str = str.replace(/%EOL%/g, "");
			str = str.replace(/\n/g, "");
		}
	}
	return str;
}

// updates selection without notifying the Java layer of the selection state
RTE.prototype.internalUpdateSelection = function() {
	if (!this.supportRichTextEditing) {
		return false;
	}	
	
	contentWindow = document.getElementById(this.editorId).contentWindow;
	this.editorDoc = contentWindow.document;
	
	if (document.all) {
		this.selection = this.editorDoc.selection;
		if (this.selection != null) {
			this.selectionRange = this.selection.createRange();
			this.reformatElementLinks();
		}
	}
	else {
		this.selection = contentWindow.getSelection();
		if (this.selection != null) {
			this.selectionRange = this.selection.getRangeAt(this.selection.rangeCount - 1).cloneRange();
			if (this.selectionRange.startContainer.nodeName == "HTML" &&
					this.selectionRange.endContainer.nodeName == "HTML") {
				// Mozilla selects the whole document when there's no RTE content, so select just the body
				this.selectionRange = this.editorDoc.createRange();
				this.selectionRange.setStart(this.editorDoc.body, 0);
				this.selectionRange.setEnd(this.editorDoc.body, 0);
			}
		}
	}
	return true;
}

// Updates the current selection and selection range.
RTE.prototype.updateSelection = function() {
	if (!this.supportRichTextEditing) {
		return false;
	}	
	
	contentWindow = document.getElementById(this.editorId).contentWindow;
	this.editorDoc = contentWindow.document;
	
	var tempSelRange;
	var selOffsetStart = 0;
	var selectedText = "";
	var fontName = "";
	var fontSize = "";
	var blockStyle = "";
	var textFlags = 0;
	
	
	if (document.all) {
		this.selection = this.editorDoc.selection;
		if (this.selection != null) {
			this.selectionRange = this.selection.createRange();
			if (this.selectionRange != null && this.selection.type != "Control") {
				tempSelRange = this.selectionRange.duplicate();
			}
			this.reformatElementLinks();
		}
	}
	else {
		this.selection = contentWindow.getSelection();
		if (this.selection != null) {
			this.selectionRange = this.selection.getRangeAt(this.selection.rangeCount - 1).cloneRange();
			tempSelRange = this.selectionRange.cloneRange();
		}
	}
	if (tempSelRange != null) {
		try {
			if (document.all) {
				if (this.selectionRange.text) {
					selectedText = this.selectionRange.text;
				}
				/* for getting this.selection offset - commented because we can't select the
				 * proper location in the HTML source tab because JTidy's reformatting of the HTML
				var html = this.getHTML();
	            var tempSelLen = tempSelRange.htmlText.length;			
	            tempSelRange.moveStart('character', -html.length);
	            selOffsetStart = tempSelRange.htmlText.length - tempSelLen;
	            */
				var selParent = tempSelRange.parentElement();
				fontName = tempSelRange.queryCommandValue('fontName');
				fontSize = tempSelRange.queryCommandValue('fontSize');
				blockStyle = tempSelRange.queryCommandValue('formatBlock');
				if (blockStyle == "Normal") {
					if (selParent.className == "quote") {
						blockStyle = "<quote>";
					} else if (selParent.className == "codeSample") {
						blockStyle = "<code>";
					} else {
						blockStyle = "<p>";
					}
				} else if (blockStyle == "Heading 3") {
					blockStyle = "<h3>";
				} else if (blockStyle == "Heading 4") {
					blockStyle = "<h4>";
				} else if (blockStyle == "Heading 5") {
					blockStyle = "<h5>";
				} else if (blockStyle == "" || blockStyle == null) {
					blockStyle = "<p>";
				}
				if (tempSelRange.queryCommandValue('bold') == true) {
					textFlags |= this.STYLE_BOLD;
				}
				if (tempSelRange.queryCommandValue('italic') == true) {
					textFlags |= this.STYLE_ITALIC;
				}
				if (tempSelRange.queryCommandValue('underline') == true) {
					textFlags |= this.STYLE_UNDERLINE;
				}
				if (tempSelRange.queryCommandValue('subscript') == true) {
					textFlags |= this.STYLE_SUBSCRIPT;
				}
				if (tempSelRange.queryCommandValue('superscript') == true) {
					textFlags |= this.STYLE_SUPERSCRIPT;
				}
				this.setStatus(this.STATUS_SELECT_TEXT, /* selOffsetStart + "$" + */
						fontName + "$" + fontSize + "$" + blockStyle + "$" + textFlags + "$" + selectedText);
			} else {
				if (this.selectionRange != null) {
					selectedText = this.selectionRange.toString();
				}
				var selParent = this.selection.focusNode;
				fontName = this.editorDoc.queryCommandValue('fontName');
				if (fontName == "") {
					fontName = "default";
				}
				fontSize = this.editorDoc.queryCommandValue('fontSize');
				if (fontSize == "") {
					fontSize = "default";
				}
				blockStyle = this.editorDoc.queryCommandValue('formatBlock');
				if (blockStyle == "p") {
					if (selParent.parentNode.className == "quote") {
						blockStyle = "<quote>";
					} else if (selParent.parentNode.className == "codeSample") {
						blockStyle = "<code>";
					} else {
						blockStyle = "<p>";
					}
				} else if (blockStyle == "h3") {
					blockStyle = "<h3>";
				} else if (blockStyle == "h4") {
					blockStyle = "<h4>";
				} else if (blockStyle == "h5") {
					blockStyle = "<h5>";
				} else if (blockStyle == "") {
					blockStyle = "<p>";
				}
				if (this.editorDoc.queryCommandState('bold') == true) {
					textFlags |= this.STYLE_BOLD;
				}
				if (this.editorDoc.queryCommandState('italic') == true) {
					textFlags |= this.STYLE_ITALIC;
				}
				if (this.editorDoc.queryCommandState('underline') == true) {
					textFlags |= this.STYLE_UNDERLINE;
				}
				if (this.editorDoc.queryCommandState('subscript') == true) {
					textFlags |= this.STYLE_SUBSCRIPT;
				}
				if (this.editorDoc.queryCommandState('superscript') == true) {
					textFlags |= this.STYLE_SUPERSCRIPT;
				}
				this.setStatus(this.STATUS_SELECT_TEXT, /* selOffsetStart + "$" + */
						fontName + "$" + fontSize + "$" + blockStyle + "$" + textFlags + "$" + selectedText);
			}
		} catch (e) {}
	}	

	return true;
}

// Sets focus to this editor.
RTE.prototype.setFocus = function() {
	if (!this.supportRichTextEditing) {
		return;
	}	
	contentWindow = document.getElementById(this.editorId).contentWindow;
	contentWindow.focus();
	this.setStatus(this.STATUS_EXEC_CMD, 1);	
}

// Reformats element links created via drag & drop.
RTE.prototype.reformatElementLinks = function() {
	var linksReformatted = 0;
	var elements = this.editorDoc.getElementsByTagName('A');
	for (var i = 0; i < elements.length; i++) {
		var element = elements[i];
		if (element.className.toLowerCase() == 'elementlink' ||
				element.className.toLowerCase() == 'elementlinkwithtype' ||
				element.className.toLowerCase() == 'elementlinkwithusertext') {
 			if (element.firstChild != null && element.firstChild.firstChild != null &&
 				element.firstChild.firstChild.firstChild != null) {
 				var linkText = element.firstChild.firstChild.firstChild.nodeValue;
 				element.removeChild(element.firstChild);
 				element.appendChild(this.editorDoc.createTextNode(linkText));
 				linksReformatted++;
 			}
		}
	}
	if (linksReformatted > 0) {
		this.setStatus(this.STATUS_REFORMAT_LINKS, null);
	}
}

// Formats the selected text.
RTE.prototype.formatText = function(command, option) {
	if (!this.readOnly && this.internalUpdateSelection()) {
		if (this.editorDoc.execCommand(command, false, option)) {
			this.setStatus(this.STATUS_EXEC_CMD, 1);		
			this.setStatus(this.STATUS_MODIFIED, null);
		}
	}
}

// Adds HTML.
RTE.prototype.addHTML = function(html) {
	if (!this.readOnly && html != "")  {
		html = this.decodeString(html);
		if (this.internalUpdateSelection()) {
			if (document.all) {
				if (this.selectionRange.text != null) {
					this.selectionRange.pasteHTML(html);
					this.setStatus(this.STATUS_EXEC_CMD, 1);
					this.setStatus(this.STATUS_MODIFIED, null);
				}
			}
			else {
				this.selectionRange.deleteContents();
				var documentFragment = this.selectionRange.createContextualFragment(html);
				this.selectionRange.insertNode(documentFragment);
				this.setStatus(this.STATUS_EXEC_CMD, 1);
				this.setStatus(this.STATUS_MODIFIED, null);
			}
		}
	}
}

// Adds an image.
RTE.prototype.addImage = function(url, height, width, alt) {
	if (this.internalUpdateSelection()) {
		if (document.all) {
			if (url != null && url != '') {
				this.formatText('insertimage', url);
			}
			if (this.selection != null && this.selection.type == 'Control' && this.selectionRange != null) {
				if (height != null && height != '') this.selectionRange.item().height = height;
				if (width != null && width != '') this.selectionRange.item().width = width;
				if (alt != null) this.selectionRange.item().alt = alt;		
			}
		} else {
			var START_MARKER = "A_-_-_";
			var END_MARKER = ":.:.:";
				// mark img links with START_MARKER + id + END_MARKER in the id, for later recovery
			var elements = this.editorDoc.getElementsByTagName('img');
			for (var i = 0; i < elements.length; i++) {
				var element = elements[i];
				element.id = START_MARKER + element.id + END_MARKER;
			}
			if (url != null && url != '') {
				this.formatText('insertimage', url);
			}
			if (this.internalUpdateSelection()) {
				var regExID = new RegExp(START_MARKER + "(.*?)" + END_MARKER);
				var elements = this.editorDoc.getElementsByTagName('img');
				for (var i = 0; i < elements.length; i++) {
					var element = elements[i];
					var id = element.id;
					if (id != null && id != '') {
						RegExp.lastIndex=0;
						var matchArray = id.match(regExID);
						if (matchArray != null && matchArray.length > 0) {
							var newId = matchArray[1];
							if (newId.length > 0) {
								element.id = newId;
							} else {
								element.removeAttribute('id');
							}
						}
					} else {
						// no id, must be the new img
						if (height != null && height != '') element.height = height;
						if (width != null && width != '') element.width = width;
						if (alt != null) element.alt = alt;		
					}
				}
			}
		}
		this.setStatus(this.STATUS_MODIFIED, null);
	}
}

// Adds a horizontal line.
RTE.prototype.addLine = function() {
	this.formatText('inserthorizontalrule', null);
}

// Adds a link.
RTE.prototype.addLink = function(url) {
	if (!this.readOnly && url != null && url != '' && this.internalUpdateSelection()) {
		if (document.all) {
			if (this.selectionRange.text == null || this.selectionRange.text == '') {
				this.selectionRange.text = url;
				this.setStatus(this.STATUS_EXEC_CMD, 1);
				this.setStatus(this.STATUS_MODIFIED, null);
			}
			else if (this.selectionRange.execCommand('createlink', false, url)) {
				this.setStatus(this.STATUS_EXEC_CMD, 1);
				this.setStatus(this.STATUS_MODIFIED, null);
			}
		}
		else {
			if (this.selection == null || this.selection == "") {		
				var urlTextNode = this.editorDoc.createTextNode(url);
				insertNodeAtSelection(document.getElementById(editorFrameId).contentWindow, urlTextNode);
			}			
			if (this.editorDoc.execCommand('createlink', false, url)) {
				this.setStatus(this.STATUS_EXEC_CMD, 1);
				this.setStatus(this.STATUS_MODIFIED, null);
			}
		}
	}
}

// Adds an ordered list.
RTE.prototype.addOrderedList = function() {
	this.formatText('insertorderedlist', null);
}

// Adds a table.
RTE.prototype.addTable = function(rows, cols, width, summary, caption, tableheaders) {
	if (this.readOnly) return;
	if (rows == 0) rows = 2;
	if (cols == 0) cols = 2;
	if (width == 0) width = "85%";
	if (this.internalUpdateSelection()) {
		var table = this.editorDoc.createElement("table");
		table.cellPadding = "2";
		table.cellSpacing = "0";
		table.border = "1";
		table.width = width;
		table.title = "";
		if (summary != null && summary != '') {
			table.summary = summary;
		}
		if (caption != null && caption != '') {
			table.title = caption;
			table.createCaption();
			var captionNode = this.editorDoc.createTextNode(caption);
			table.caption.appendChild(captionNode);
		}
		tbody = this.editorDoc.createElement("tbody");
		for (var i = 0; i < rows; i++) {
			tr = this.editorDoc.createElement("tr");
			for (var j = 0; j < cols; j++) {
				if (i == 0 && (tableheaders == this.TABLE_HEADERS_COLS || tableheaders == this.TABLE_HEADERS_BOTH)) {
					th = this.editorDoc.createElement("th");
					th.scope = "col";
					th.id = "";
					th.abbr = th.id;
					var headerNode = this.editorDoc.createTextNode(th.id);
					th.appendChild(headerNode);
					if (!document.all) {
						br = this.editorDoc.createElement("br");
						th.appendChild(br);
					}
					tr.appendChild(th);
				}
				else if (j == 0 && (tableheaders == this.TABLE_HEADERS_ROWS || tableheaders == this.TABLE_HEADERS_BOTH)) {
					th = this.editorDoc.createElement("th");
					th.scope = "row";
					th.id = "";
					th.abbr = th.id;
					var headerNode = this.editorDoc.createTextNode(th.id);
					th.appendChild(headerNode);
					if (!document.all) {
						br = this.editorDoc.createElement("br");
						th.appendChild(br);
					}
					tr.appendChild(th);
				}
				else {
					td = this.editorDoc.createElement("td");
					if (!document.all) {
						br = this.editorDoc.createElement("br");
						td.appendChild(br);
					}
					tr.appendChild(td);
				}
			}
			tbody.appendChild(tr);
    	}
		table.appendChild(tbody);
		if (document.all) {
			this.selectionRange.parentElement().appendChild(table);
		}
		else {
			this.selectionRange.insertNode(table);
		}
		this.setStatus(this.STATUS_EXEC_CMD, 1);
		this.setStatus(this.STATUS_MODIFIED, null);			
	}
}

// Adds an unordered list.
RTE.prototype.addUnorderedList = function() {
	this.formatText('insertunorderedlist', null);
}

// Sets the background color of the selected text.
RTE.prototype.backColor = function(color) {
	if (color != null && color != '') {
		this.formatText('backcolor', color);
	}
}

// Toggles the 'bold' attribute of the selected text.
RTE.prototype.bold = function() {
	this.formatText('bold', null);
}

// Copies the selected text to the clipboard.
RTE.prototype.copy = function() {
	if (this.internalUpdateSelection()) {
		if (this.editorDoc.execCommand('copy', false, null)) {
			this.setStatus(this.STATUS_EXEC_CMD, 1);
		}
	}
}

// Cuts the selected text to the clipboard.
RTE.prototype.cut = function() {
	this.formatText('cut', null);
}

// Deletes the selected text.
RTE.prototype.deleteText = function() {
	this.formatText('delete', null);
}

// Finds text.
RTE.prototype.findText = function(text, dir, options) {
	if (text == null || text == "") {
		return;
	}
	else {
		text = this.decodeString(text);
	}
	
	if (this.internalUpdateSelection()) {
		if (document.all) {
			this.selectionRange.collapse(dir < 0);
			if (this.selectionRange.findText(text, dir, options)) {
				this.selectionRange.scrollIntoView();
				this.selectionRange.select();
				this.selectionRange.collapse(dir < 0);
				this.setStatus(this.STATUS_EXEC_CMD, 1);
			}
		}
		else {	
			// find(text, caseSensitive, backwards, wrapAround, wholeWord, searchInFrames, showDialog)
			var caseSensitive = true;
			var backwards = false;
			var wholeWord = true;
			if ((options & 4) == 0) caseSensitive = false;
			if (dir == -1) backwards = true;
			if ((options & 2) == 0) wholeWord = false;
			if (contentWindow.find(text, caseSensitive, backwards, false, wholeWord, false, false)) {
				this.setStatus(this.STATUS_EXEC_CMD, 1);
			}
		}
	}
}

// Sets the foreground color of the selected text.
RTE.prototype.foreColor = function(color) {
	if (color != null && color != '') {
		this.formatText('forecolor', color);
	}
}

// Formats the selected text using the given HTML heading tag.
RTE.prototype.formatBlock = function(tag) {
	if (tag != null && tag != '') {
		this.formatText('formatblock', tag);
	}
}


RTE.prototype.INDENTED_LIST_BAD_HTML_IE = "</li>.*<li style=\"list-style: none\">";
RTE.prototype.INDENTED_LIST_BAD_HTML_MOZ = "</li>.*<li style=\"list-style-type: none; list-style-image: none; list-style-position: outside;\">";

// Indents the selected text.
RTE.prototype.indent = function() {
	this.formatText('indent', null);
	// fix for sub-lists
	var html = document.getElementById(this.editorId).contentWindow.document.body.innerHTML;
	if (document.all) {
		html = html.replace(this.INDENTED_LIST_BAD_HTML_IE, "");
	} else {
		// firefox sometimes puts the same as IE, sometimes more junk
		html = html.replace(this.INDENTED_LIST_BAD_HTML_IE, "");
		html = html.replace(this.INDENTED_LIST_BAD_HTML_MOZ, "");
	}
	this.setText(html);
}

// Toggles the 'italic' attribute of the selected text.
RTE.prototype.italic = function() {
	this.formatText('italic', null);
}

// Center justifies the selected text.
RTE.prototype.justifyCenter = function() {
	this.formatText('justifycenter', null);
}

// Fully justifies the selected text.
RTE.prototype.justifyFull = function() {
	this.formatText('justifyfull', null);
}

// Left justifies the selected text.
RTE.prototype.justifyLeft = function() {
	this.formatText('justifyleft', null);
}

// Right justifies the selected text.
RTE.prototype.justifyRight = function() {
	this.formatText('justifyright', null);
}

// Outdents the selected text.
RTE.prototype.outdent = function() {
	this.formatText('outdent', null);
}

// Pastes text from the clipboard.
RTE.prototype.paste = function(sourceURL) {
	if (sourceURL == null) {
		sourceURL = "";
	}
	else {
		sourceURL = this.decodeString(sourceURL);
	}
	if (document.all) {
		var START_MARKER = "A_-_-_";
		var END_MARKER = ":.:.:";
		// mark img and <a /> links with START_MARKER + src/href + END_MARKER in the id, for later recovery
		var elements = this.editorDoc.getElementsByTagName('img');
		for (var i = 0; i < elements.length; i++) {
			var element = elements[i];
			var id = element.id;
			element.id = START_MARKER + element.src + END_MARKER + id;
		}
		var elements = this.editorDoc.getElementsByTagName('a');
		for (var i = 0; i < elements.length; i++) {
			var element = elements[i];
			var id = element.id;
			element.id = START_MARKER + element.href + END_MARKER + id;
		}

		// change the <base> of the document
		var oldBaseHREF = this.editorDoc.getElementsByTagName('base')[0].href;
		this.editorDoc.getElementsByTagName('base')[0].href = sourceURL;

		this.formatText('paste', null);
		
		// restore <base>
		this.editorDoc.getElementsByTagName('base')[0].href = oldBaseHREF;
	}
	else {
		this.setStatus(this.STATUS_EXEC_CMD, 1);
		this.setStatus(this.STATUS_MODIFIED, null);
	}
	if (this.internalUpdateSelection()) {
		try {
			var regExRes = new RegExp("file\:([^=]+)(/resources/)(.+)", "g");
			var regExRef = new RegExp("(.+)(#.+)");
			var regEx = new RegExp("file\:([^=]+)/([^/]+)", "g");	
			var regExID = new RegExp(START_MARKER + "(.*?)" + END_MARKER + "(.*?)");
			var elements = this.editorDoc.getElementsByTagName('img');
			for (var i = 0; i < elements.length; i++) {
				var element = elements[i];
				var id = element.id;
				if (id != null && id != '') {
					RegExp.lastIndex=0;
					var matchArray = id.match(regExID);
					if (matchArray != null && matchArray.length > 1) {
						element.src = matchArray[1];
						if (matchArray.length > 2 && matchArray[2].length > 0) {
							element.id = matchArray[2];
						}
						else {
							element.removeAttribute('id');
						}
						continue;
					}
				}
				var src = element.src;
				if (src != null && src != '') {
					if (src.indexOf('about:./resources') != -1) {
						// fix for IE 7 when pasting from another RTE
						// IE7 resolves these as "about:./resources/<file>"
						// so remove the "about:."
						src = src.replace("about:", "");
					}
					if (src.indexOf('about:resources') != -1) {
						// fix for IE 7 when pasting from another RTE
						// IE7 sometimes resolves these as "about:resources/<file>"
						// so remove the "about:" and put in "./"
						src = src.replace("about:", "./");
					}
					if (src.indexOf('resources') != -1) {
						element.src = src.replace(regExRes, "./resources/$3");
					}
					else {
						element.src = src.replace(regEx, "./resources/$2");
					}
				}
			}
			var elements = this.editorDoc.getElementsByTagName('a');
			for (var i = 0; i < elements.length; i++) {
				var element = elements[i];
				var id = element.id;
				if (id != null && id != '') {
					RegExp.lastIndex=0;
					var matchArray = id.match(regExID);
					if (matchArray != null && matchArray.length > 1) {
						element.href = matchArray[1];
						if (matchArray.length > 2 && matchArray[2].length > 0) {
							element.id = matchArray[2];
						}
						else {
							element.removeAttribute('id');
						}
						continue;
					}
				}
				var href = element.href;
				if (href != null && href != '') {
					// fix self-referencing hrefs
					if (href.indexOf('#') != -1) {
						RegExp.lastIndex=0;
						var matchArray = href.match(regExRef);
						if (matchArray != null && matchArray.length > 2) {
							var hrefFile = matchArray[1];
							var ref = matchArray[2];
							if (hrefFile == sourceURL) {
								element.href = ref;
								continue;
							}
						}
					}
					// fix hrefs already in resources
					if (href.indexOf('resources') != -1) {
						element.href = href.replace(regExRes, "./resources/$3");
					}
					// fix hrefs not in resources
					else {
						element.href = href.replace(regEx, "./resources/$2");
					}
				}
			}
		}
		catch (e) {
		}
	}
}

// Redo the previous command.
RTE.prototype.redo = function() {
	this.formatText('redo', null);
}

// Redo the previous command.
RTE.prototype.removeFormat = function() {
	this.formatText('removeformat', null);
}



RTE.prototype._replaceAllText = function(findText, replaceText, options) {
	// this is IE only
	if (document.all) {
		var tempRange =  document.getElementById(this.editorId).contentWindow.document.body.createTextRange();
		tempRange.moveStart('character', -10000000000);
		do {
			tempRange.collapse();
			if (tempRange.findText(findText, 10000000000, options)) {
				tempRange.text = replaceText;
				tempRange.select();
			} else {		
				break;
			}
		} while (true);
	}
}

// Replaces all text.
RTE.prototype.replaceAllText = function(findText, replaceText, options) {
	if (this.readOnly || findText == null || findText == "") {
		return;
	}
	else {
		findText = this.decodeString(findText);
	}
	if (replaceText == null) {
		replaceText = "";
	}
	else {
		replaceText = this.decodeString(replaceText);
	}
	
	if (document.all) {
		// TODO: Move the insertion point to the start of the HTML
		// and perform a search and replace in the forward direction. 
		this._replaceAllText(findText, replaceText, options);
	}
	else {
		// TODO: Emulate the IE implementation.
		var html = document.getElementById(this.editorId).contentWindow.document.body.innerHTML;
		var optionStr = "/g";
		if ((options & 4) == 0) {
			optionStr += "i";
		}
		var regExp = eval("/" + findText + optionStr);
		html = html.replace(regExp, replaceText);
		this.setText(html);
	}
	
	this.setStatus(this.STATUS_EXEC_CMD, 1);
	this.setStatus(this.STATUS_MODIFIED, null);
}

// Replaces text.
RTE.prototype.replaceText = function(replaceText, dir, options) {
	if (this.readOnly || !this.internalUpdateSelection()) {
		return;
	}
	if (replaceText == null) {
		replaceText = "";
	}
	else {
		replaceText = this.decodeString(replaceText);
	}
	if (document.all) {
		this.selectionRange.text = replaceText;
		if (replaceText != "") {
			this.selectionRange.moveStart("word", -1);
			this.selectionRange.select();
			this.selectionRange.collapse(dir < 0);
		}
	}
	else {
		this.selectionRange.deleteContents();
		this.selectionRange.insertNode(this.editorDoc.createTextNode(replaceText));
	}
	this.setStatus(this.STATUS_EXEC_CMD, 1);
	this.setStatus(this.STATUS_MODIFIED, null);
}

// Selects all text.
RTE.prototype.selectAll = function() {
	if (this.internalUpdateSelection()) {
		if (this.editorDoc.execCommand('selectall', false, null)) {
			this.setStatus(this.STATUS_EXEC_CMD, 1);
		}
	}
}

// Sets the font name for the selected text.
RTE.prototype.setFontName = function(name) {
	if (this.internalUpdateSelection()) {
		if (name != null) {
			if (name == '') {
				this.formatText('removeFormat');
			} else {
				this.formatText('fontname', name);
			}
		}
	}
}

// Sets the font size for the selected text.
RTE.prototype.setFontSize = function(size) {
	if (this.internalUpdateSelection()) {
		if (size != null) {
			if (size == '') {
				this.formatText('removeFormat');
			} else {
				this.formatText('fontsize', size);
			}
		}
	}
}

// Sets the font style for the selected text.
RTE.prototype.setFontStyle = function(style) { 
	if (!this.readOnly && style != null && style != '' && this.internalUpdateSelection()) {
		try {
			if (document.all) {
				this.selectionRange.execCommand("removeformat");
				this.selectionRange.parentElement().removeAttribute("className");
			}
		}
		catch (e) {
		}
		if (style == "<quote>") {
			this.formatText('formatblock', '<p>');
			if (document.all) {
				this.selectionRange.parentElement().className = "quote";
			}
			else {
				this.selection.focusNode.parentNode.className = "quote";
			}
		}
		else if (style == "<code>") {
			this.formatText('formatblock', '<p>');
			if (document.all) {
				this.selectionRange.parentElement().className = "codeSample";
			}
			else {
				this.selection.focusNode.parentNode.className = "codeSample";
			}
		}
		else {
			if (!document.all && style == "<p>") {
				// A hack to get rid of the "className" attribute in Mozilla/Firefox.
				this.formatText('formatblock', '<h4>');
			}
			this.formatText('formatblock', style);
		}
	}
}

// Sets whether the content can be edited.
RTE.prototype.setEditable = function(editable) {
	var doc = document.getElementById(this.editorId).contentWindow.document;
    if (editable != null && editable == 'true') {
		if (document.all) {
			doc.body.contentEditable = "true";
		}
		else {
			doc.designMode = "on";
		}
		this.readOnly = false;
	}
	else {
		if (document.all) {		
			doc.body.contentEditable = "false";
		}
		else {
			doc.designMode = "off";
		}
		this.readOnly = true;
	}
	this.setStatus(this.STATUS_EXEC_CMD, 1);	
}

// Toggles the 'strike-through' attribute of the selected text.
RTE.prototype.strikeThrough = function() {
	this.formatText('strikethrough', size);
}

// Toggles the 'subscript' attribute of the selected text.
RTE.prototype.subscript = function() {
	this.formatText('subscript', null);
}

// Toggles the 'superscript' attribute of the selected text.
RTE.prototype.superscript = function() {
	this.formatText('superscript', null);
}

// Toggles the 'underline' attribute of the selected text.
RTE.prototype.underline = function() {
	this.formatText('underline', null);
}

// Converts a link to normal text.
RTE.prototype.unlink = function() {
	this.formatText('unlink', null);
}

function ObjToString(object) {
	var ret = "Object " + object.name + " is [\n";
	for (var prop in object) {
		ret += "  " + prop + " is " + object[prop] + ";\n";
	}
	return ret + "]";
}
