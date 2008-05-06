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
	
	<xsl:include href="guidance.xsl"/>	

	<xsl:template match="/Element">
		<xsl:variable name="elementType" select="@Type"/>
		<xsl:variable name="elementTypeName" select="@TypeName"/>
		<xsl:variable name="elementName" select="@Name"/>
		<xsl:variable name="elementPresentationName" select="@DisplayName"/>
		<xsl:variable name="backPath" select="@BackPath"/>
		<xsl:variable name="imagePath" select="concat($backPath, 'images/')"/>		
	    <xsl:variable name="shapeImage" select="concat($backPath,@ShapeiconUrl)"/>
		<xsl:variable name="presentation" select="reference[@name='presentation']"/>
		<xsl:variable name="contentDescription" select="$presentation/Element"/>
		<xsl:variable name="copyright" select="copyright"/>
		<xsl:variable name="showTreeBrowser" select="@showTreeBrowser"/>
		<xsl:variable name="responsibleRole" select="reference[@name='responsibleRole']/Element[@Type='Role']"/>

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
				<meta name="element_type" content="{$elementType}"/>
				<meta name="filetype" content="description"/>
				<meta name="role">
				<xsl:attribute name="content"><xsl:value-of select="$responsibleRole/@DisplayName"/></xsl:attribute>
				</meta>
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
					contentPage.preload(imgPath, backPath, nodeInfo,  '', false, false, false);
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
							<xsl:call-template name="relationshipsSection2"/>
							<xsl:call-template name="descriptionSection">
								<xsl:with-param name="description" select="$contentDescription"/>
							</xsl:call-template>
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

	<xsl:template name="relationshipsSection2">
		<xsl:variable name="subPractices" select="referenceList[@name='subPractices']/Element"/>
		<xsl:variable name="inputSlots" select="referenceList[@name='Input work product slots']/Element"/>
		<xsl:variable name="contentReferences" select="referenceList[@name='contentReferences']/Element"/>
		<xsl:variable name="activityReferences" select="referenceList[@name='activityReferences']/Element"/>
		<xsl:variable name="categories" select="referenceList[@name='ContentElement_CustomCategories']/Element"/>
		<xsl:variable name="practiceTree" select="referenceList[@name='Practice guidance tree']"/>
		<xsl:if test="count($contentReferences) + count($subPractices) + count($activityReferences) + count($categories) + count($inputSlots) > 0">
			<div class="sectionHeading"><xsl:value-of select="$relationshipsText"/></div>
			<div class="sectionContent">
				<table class="sectionTable" border="0" cellspacing="0" cellpadding="0">
					<xsl:call-template name="addReferences">
						<xsl:with-param name="refName" select="$categoriesText"/>
						<xsl:with-param name="refElement" select="$categories"/>
					</xsl:call-template>
					<tr valign="top">
						<th class="sectionTableHeading" scope="row">
							<xsl:value-of select="$relationshipsText"/>
						</th>
						<td class="sectionTableCell">
							<xsl:for-each select="$practiceTree/*">
							<ul>
								<xsl:if test="name()='Element'">
								<li>
									<img>
										<xsl:attribute name="src">./../../../<xsl:value-of select="@ShapeiconUrl"/></xsl:attribute>
										<xsl:attribute name="height">16</xsl:attribute>
										<xsl:attribute name="width">16</xsl:attribute>
								  	</img>
								  	<a>
										<xsl:attribute name="href"><xsl:value-of select="@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute>
										<xsl:value-of disable-output-escaping="yes" select="@DisplayName"/>
								  	</a>
								</li>
								</xsl:if>
								<xsl:if test="name()='referenceList'">
								<li>
									<xsl:value-of select="@name"/>
								</li>
								<ul>
									<xsl:for-each select="./*">
									<xsl:if test="name()='referenceList'">
									<li>
									  <xsl:value-of select="@name"/>
									</li>
									<ul>
										<xsl:for-each select="./*">
											<li>
												<img>
													<xsl:attribute name="src">./../../../<xsl:value-of select="@ShapeiconUrl"/></xsl:attribute>
													<xsl:attribute name="height">16</xsl:attribute>
													<xsl:attribute name="width">16</xsl:attribute>
											  	</img>
											  	<a>
													<xsl:attribute name="href"><xsl:value-of select="@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute>
													<xsl:value-of disable-output-escaping="yes" select="@DisplayName"/>
											  	</a>
											</li>
										</xsl:for-each>
									</ul>
									</xsl:if>
									<xsl:if test="name()='Element'">
									<li>
										<img>
											<xsl:attribute name="src">./../../../<xsl:value-of select="@ShapeiconUrl"/></xsl:attribute>
											<xsl:attribute name="height">16</xsl:attribute>
											<xsl:attribute name="width">16</xsl:attribute>
									  	</img>
									  	<a>
											<xsl:attribute name="href"><xsl:value-of select="@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute>
											<xsl:value-of disable-output-escaping="yes" select="@DisplayName"/>
									  	</a>
									</li>
									</xsl:if>
									</xsl:for-each>
								</ul>
								</xsl:if>
						  </ul> 		
						  </xsl:for-each>
				  	</td>
					</tr>	
					
					
					<xsl:call-template name="addReferences">
						<xsl:with-param name="refName" select="$inputsText"/>
						<xsl:with-param name="refElement" select="$inputSlots"/>
					</xsl:call-template>					
				</table>
			</div>
		</xsl:if>
    </xsl:template>

	<xsl:template name="descriptionSection">
		<xsl:param name="description"/>
		<xsl:variable name="mainDescription" select="$description/attribute[@name='mainDescription']"/>
		<xsl:variable name="problem" select="$description/attribute[@name='problem']"/>
		<xsl:variable name="background" select="$description/attribute[@name='background']"/>
		<xsl:variable name="goals" select="$description/attribute[@name='goals']"/>
		<xsl:variable name="application" select="$description/attribute[@name='application']"/>
		<xsl:variable name="levelsOfAdoption" select="$description/attribute[@name='levelsOfAdoption']"/>
		<xsl:variable name="additionalInfo" select="$description/attribute[@name='additionalInfo']"/>
		
		<xsl:if test="$mainDescription != '' or $problem != '' or $background != '' or $goals != '' or $application != '' or $levelsOfAdoption != '' or $additionalInfo != ''">
			<div class="sectionHeading"><xsl:value-of select="$descriptionText"/></div>
			<div class="sectionContent">			
				<table class="sectionTable" border="0" cellspacing="0" cellpadding="0">
					<xsl:if test="$problem != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$problemText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$problem"/>
							</td>
						</tr>
					</xsl:if>
					<xsl:if test="$goals != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$goalsText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$goals"/>
							</td>
						</tr>
					</xsl:if>
					<xsl:if test="$background != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$backgroundText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$background"/>
							</td>
						</tr>
					</xsl:if>
					<xsl:if test="$mainDescription != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$mainDescriptionText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$mainDescription"/>
							</td>
						</tr>
					</xsl:if>
					<xsl:if test="$application != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$applicationText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$application"/>
							</td>
						</tr>
					</xsl:if>
					<xsl:if test="$levelsOfAdoption != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$levelsOfAdoptionText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$levelsOfAdoption"/>
							</td>
						</tr>
					</xsl:if>
					<xsl:if test="$additionalInfo != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$additionalInfoText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$additionalInfo"/>
							</td>
						</tr>
					</xsl:if>
				</table>
			</div>
		</xsl:if>
	</xsl:template>    

</xsl:stylesheet>
