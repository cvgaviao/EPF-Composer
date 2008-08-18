<?xml version="1.0" encoding="UTF-8"?>
<!-- 
    Copyright (c) 2005, 2007 IBM Corporation and others.
    All rights reserved. This program and the accompanying materials
    are made available under the terms of the Eclipse Public License v1.0
    which accompanies this distribution, and is available at
    http://www.eclipse.org/legal/epl-v10.html
    Contributors:
    IBM Corporation - initial implementation
-->

<xsl:stylesheet version="1.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">

<xsl:output method="html" version="1.0" encoding="UTF-8" 
	doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" 
	doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" 
	indent="yes"/>
	
	<xsl:include href="workproduct.xsl"/>
	<xsl:include href="mapping.xsl"/>
	<xsl:include href="main_description.xsl"/>	

	<xsl:template match="/Element">
		<xsl:variable name="elementType" select="@Type"/>
		<xsl:variable name="elementTypeName" select="@TypeName"/>
		<xsl:variable name="elementName" select="@Name"/>
		<xsl:variable name="elementPresentationName" select="@DisplayName"/>
		<xsl:variable name="backPath" select="@BackPath"/>
		<xsl:variable name="imagePath" select="concat($backPath, 'images/')"/>
	    <xsl:variable name="shapeImage" select="concat($backPath,@ShapeiconUrl)"/>
		<xsl:variable name="presentation" select="reference[@name='presentation']"/>
		<xsl:variable name="deliverableDescription" select="$presentation/Element[@Type='DeliverableDescription']"/>
		<xsl:variable name="copyright" select="copyright"/>
		<xsl:variable name="showTreeBrowser" select="@showTreeBrowser"/>
		<xsl:variable name="responsibleRole" select="reference[@name='responsibleRole']/Element[@Type='Role']"/>
		<xsl:variable name="tagValues" select="@TagValues"/>

		<html>
			<xsl:attribute name="lang"><xsl:value-of select="@lang"/></xsl:attribute>
			<xsl:attribute name="xml:lang"><xsl:value-of select="@lang"/></xsl:attribute>
			<head>
				<title><xsl:value-of select="$elementTypeName"/>: <xsl:value-of select="$elementPresentationName"/></title>
				<xsl:call-template name="umaMetaTags">
					<xsl:with-param name="elementType" select="$elementType"/>
					<xsl:with-param name="elementName" select="$elementName"/>
					<xsl:with-param name="elementPresentationName" select="$elementPresentationName"/>
				</xsl:call-template>
				<meta name="element_type"> 
					<xsl:call-template name="mapping">
						<xsl:with-param name="elementType" select="$elementType"/>
					</xsl:call-template>
				</meta>
				<!-- <meta name="element_type" content="{$elementType}"/> -->
				<meta name="filetype" content="description"/>
				<meta name="role">
					<xsl:attribute name="content"><xsl:value-of select="$responsibleRole/@DisplayName"/></xsl:attribute>
				</meta>
				<xsl:if test="$tagValues!=''">
				    <meta name="tags" content="{$tagValues}"/>
				</xsl:if>
				<link rel="StyleSheet" href="{$backPath}css/default.css" type="text/css"/>
				<script src="{$backPath}scripts/ContentPageResource.js" type="text/javascript" language="JavaScript"></script>
				<script src="{$backPath}scripts/ContentPageSection.js" type="text/javascript" language="JavaScript"></script>
				<script src="{$backPath}scripts/ContentPageSubSection.js" type="text/javascript" language="JavaScript"></script>
				<script src="{$backPath}scripts/ContentPageToolbar.js" type="text/javascript" language="JavaScript"></script>
				<script src="{$backPath}scripts/contentPage.js" type="text/javascript" language="JavaScript"></script>
				<script type="text/javascript" language="JavaScript">
					var backPath = '<xsl:value-of select="$backPath"/>';
					var imgPath = '<xsl:value-of select="$imagePath"/>';
					var nodeInfo=null;
					contentPage.preload(imgPath, backPath, nodeInfo, '', false, false, false);
				</script>
			</head>
			<body>
			<div id="breadcrumbs"></div>
				<table border="0" cellpadding="0" cellspacing="0" width="100%">
					<tr>
						<td valign="top">
							<a name="Top"/>
							<xsl:call-template name="overview">
								<xsl:with-param name="elementType" select="$elementType"/>
								<xsl:with-param name="elementTypeName" select="$elementTypeName"/>
								<xsl:with-param name="elementPresentationName" select="$elementPresentationName"/>
								<xsl:with-param name="elementIcon" select="$shapeImage"/>
								<xsl:with-param name="backPath" select="$backPath"/>
								<xsl:with-param name="showTreeBrowser" select="$showTreeBrowser"/>
							</xsl:call-template>
							<xsl:call-template name="purposeSection">
								<xsl:with-param name="description" select="$deliverableDescription"/>
							</xsl:call-template>
							<xsl:call-template name="relationshipsSection"/>
							<xsl:call-template name="mainDescriptionSection">
								<xsl:with-param name="description" select="$deliverableDescription"/>
							</xsl:call-template>
							<xsl:call-template name="workProductIllustrationsSection"/>
							<xsl:call-template name="deliverableSection">
								<xsl:with-param name="description" select="$deliverableDescription"/>
							</xsl:call-template>
							<xsl:call-template name="keyConsiderationsSection">
								<xsl:with-param name="description" select="$deliverableDescription"/>
							</xsl:call-template>
							<xsl:call-template name="tailoringSection">
								<xsl:with-param name="description" select="$deliverableDescription"/>
							</xsl:call-template>
							<xsl:call-template name="moreInfoSection"/>
							<xsl:call-template name="copyright">
								<xsl:with-param name="copyright" select="$copyright"/>
							</xsl:call-template>
						</td>
					</tr>
				</table>
			</body>
			<script type="text/javascript" language="JavaScript">
				contentPage.onload();
			</script>
		</html>
	</xsl:template>

	<xsl:template name="deliverableSection">
		<xsl:param name="description"/>
		<xsl:variable name="externalDescription" select="$description/attribute[@name='externalDescription']"/>
		<xsl:variable name="deliveredWorkProducts" select="referenceList[@name='deliveredWorkProducts']/Element"/>
		<xsl:variable name="packagingGuidance" select="$description/attribute[@name='packagingGuidance']"/>
		
		<xsl:if test="$externalDescription != '' or count($deliveredWorkProducts) > 0 or $packagingGuidance != ''">
			<div class="sectionHeading"><xsl:value-of select="$deliverablePropertiesText"/></div>
			<div class="sectionContent">
				<table class="sectionTable" border="0" cellspacing="0" cellpadding="0">
					<xsl:if test="$externalDescription != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$externalDescriptionText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$externalDescription"/>
							</td>
						</tr>
					</xsl:if>
					<xsl:if test="count($deliveredWorkProducts) > 0">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$deliveredPartsText"/></th>
							<td class="sectionTableCell">
							<ul>
								<xsl:for-each select="$deliveredWorkProducts">
								<xsl:sort select="@DisplayName"/>
									<li>
										<a>
											<xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute>
											<xsl:value-of select="@DisplayName"/>
										</a>
									</li>
								</xsl:for-each>
							</ul>
							</td>
						</tr>
					</xsl:if>
					<xsl:if test="$packagingGuidance != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$packagingInstructionsText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$packagingGuidance"/>
							</td>
						</tr>
					</xsl:if>
				</table>
			</div>
		</xsl:if>
	</xsl:template>

</xsl:stylesheet>
