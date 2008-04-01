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
	
	<xsl:include href="descriptor.xsl"/>
	<xsl:include href="purpose.xsl"/>
	<xsl:include href="tailoring.xsl"/>
	<xsl:include href="workproduct_helper.xsl"/>

	<xsl:template match="/Element">
		<xsl:variable name="elementType" select="@Type"/>
		<xsl:variable name="elementTypeName" select="@TypeName"/>
		<xsl:variable name="elementName" select="@Name"/>
		<xsl:variable name="elementPresentationName" select="@DisplayName"/>
		<xsl:variable name="backPath" select="@BackPath"/>
		<xsl:variable name="shapeImage" select="concat($backPath,@ShapeiconUrl)"/>
		<xsl:variable name="imagePath" select="concat($backPath, 'images/')"/>		
		<xsl:variable name="presentation" select="reference[@name='presentation']"/>				
		<xsl:variable name="contentDescription" select="$presentation/Element[@Type='WorkProductDescriptor']"/>
		<xsl:variable name="workProductDescriptor" select="$presentation/Element[@Type='WorkProductDescriptor']"/>
		<xsl:variable name="descriptorDescription" select="$presentation/Element[@Type='DescriptorDescription']"/>		
		<xsl:variable name="responsibleRole" select="reference[@name='responsibleRole']/Element[@Type='RoleDescriptor']"/>
		<xsl:variable name="copyright" select="copyright"/>
		<xsl:variable name="showTreeBrowser" select="@showTreeBrowser"/>
		<xsl:variable name="queryString" select="@queryString"/>
		<xsl:variable name="relProcessPath" select="@relProcessPath"/>

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
				<script src="{$backPath}scripts/ActivityTreeTable.js" type="text/javascript" language="JavaScript"></script>
				<script src="{$backPath}scripts/ProcessElementPage.js" type="text/javascript" language="JavaScript"></script>
				<script src="{$backPath}scripts/ContentPageToolbar.js" type="text/javascript" language="JavaScript"></script>
				<script src="{$backPath}scripts/contentPage.js" type="text/javascript" language="JavaScript"></script>
				<script src="{$backPath}scripts/processElementData.js" type="text/javascript" language="JavaScript"></script>
				<script type="text/javascript" language="JavaScript">
					var defaultQueryStr = '<xsl:value-of select="$queryString"/>';
					var backPath = '<xsl:value-of select="$backPath"/>';
					var imgPath = '<xsl:value-of select="$imagePath"/>';
					var nodeInfo=null;
					contentPage.preload(imgPath, backPath, nodeInfo, defaultQueryStr, false, true, false);
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
								<xsl:with-param name="description" select="$descriptorDescription"/>
							</xsl:call-template>
							<xsl:call-template name="descriptorRelationshipsSection"/>
							<xsl:call-template name="deliverableSection">
								<xsl:with-param name="description" select="$descriptorDescription"/>
							</xsl:call-template>

							<xsl:choose>
								<xsl:when test="$descriptorDescription/attribute[@name='briefOutline'] !='' ">
									<xsl:call-template name="descriptionSection">
										<xsl:with-param name="description" select="$descriptorDescription"/>
									</xsl:call-template>	
								</xsl:when>
								<xsl:otherwise>
									<xsl:call-template name="refinedDescriptionSection">
										<xsl:with-param name="description" select="$descriptorDescription"/>
									</xsl:call-template>	
								</xsl:otherwise>
							</xsl:choose>
							<xsl:call-template name="propertiesSection">
								<xsl:with-param name="contentDescription" select="$contentDescription"/>
							</xsl:call-template>
							<xsl:call-template name="usageSection">
								<xsl:with-param name="contentDescription" select="$descriptorDescription"/>
							</xsl:call-template>
							<xsl:call-template name="workProductIllustrationsSection"/>
							<xsl:call-template name="keyConsiderationsSection">
								<xsl:with-param name="description" select="$descriptorDescription"/>
							</xsl:call-template>
							<xsl:call-template name="tailoringSection">
								<xsl:with-param name="description" select="$descriptorDescription"/>
							</xsl:call-template>
							<xsl:call-template name="moreInfoSection"/>
							<xsl:call-template name="copyright">
								<xsl:with-param name="copyright" select="$copyright"/>
							</xsl:call-template>
						</td>						
					</tr>
				</table>
			</body>
				<script language="JavaScript" type="text/javascript">
					contentPage.onload();
					contentPage.processPage.fixDescriptorLinks();
				</script>
		</html>
	</xsl:template>	

	<xsl:template name="descriptionSection">
		<xsl:param name="description"/>
		<xsl:variable name="briefOutline" select="$description/attribute[@name='briefOutline']"/>
		<xsl:variable name="mainDescription" select="$description/attribute[@name='refinedDescription']"/>
		<xsl:if test="$briefOutline != '' or $mainDescription != ''">
			<div class="sectionHeading"><xsl:value-of select="$descriptionText"/></div>
			<div class="sectionContent">			
				<table class="sectionTable" border="0" cellspacing="0" cellpadding="0">
					<xsl:if test="$briefOutline != ''">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$briefOutlineText"/></th>
							<td class="sectionTableCell">
								<xsl:value-of disable-output-escaping="yes" select="$briefOutline"/>
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
				</table>
			</div>
		</xsl:if>
	</xsl:template>	

	<xsl:template name="propertiesSection">
		<xsl:param name="contentDescription"/>
		<div class="sectionHeading"><xsl:value-of select="$propertiesText"/></div>
		<div class="sectionContent">
			<table class="sectionTable" border="0" cellspacing="0" cellpadding="0">
			    <xsl:if test="attribute[@name='activityEntryState']">
					<xsl:call-template name="property">
						<xsl:with-param name="fieldLabel" select="$activityEntryStateText"/>
						<xsl:with-param name="fieldText" select="attribute[@name='activityEntryState']"/>
					</xsl:call-template>
				</xsl:if>
				<xsl:if test="attribute[@name='activityExitState']">
					<xsl:call-template name="property">
						<xsl:with-param name="fieldLabel" select="$activityExitStateText"/>
						<xsl:with-param name="fieldText" select="attribute[@name='activityExitState']"/>
					</xsl:call-template>
				</xsl:if>			
				<xsl:if test="attribute[@name='isOptional']">
					<xsl:call-template name="property">
						<xsl:with-param name="fieldLabel" select="$optionalText"/>
						<xsl:with-param name="fieldText" select="attribute[@name='isOptional']"/>
					</xsl:call-template>
				</xsl:if>				
				<xsl:if test="attribute[@name='isPlanned']">
					<xsl:call-template name="property">
						<xsl:with-param name="fieldLabel" select="$plannedText"/>
						<xsl:with-param name="fieldText" select="attribute[@name='isPlanned']"/>
					</xsl:call-template>
				</xsl:if>		
				<xsl:if test="attribute[@name='PlanningData']">
					<xsl:call-template name="property">
						<xsl:with-param name="fieldLabel" select="$planningDataText"/>
						<xsl:with-param name="fieldText" select="attribute[@name='PlanningData']"/>
					</xsl:call-template>
				</xsl:if>
			</table>
		</div>
	</xsl:template>

	<xsl:template name="usageSection">
		<xsl:param name="contentDescription"/>
		<xsl:variable name="usageGuidance" select="$contentDescription/attribute[@name='usageGuidance']"/>
		<xsl:if test="$usageGuidance != ''">
			<div class="sectionHeading"><xsl:value-of select="$usageGuidanceText"/></div>
			<div class="sectionContent">
				<table class="sectionTable" border="0" cellspacing="0" cellpadding="0">
					<tr valign="top">
						<td class="sectionTableCell">
							<xsl:value-of disable-output-escaping="yes" select="$usageGuidance"/>
						</td>
					</tr>
				</table>
			</div>
		</xsl:if>
	</xsl:template>

	<xsl:template name="descriptorRelationshipsSection">
		<xsl:variable name="fulfillingWorkProducts" select="referenceList[@name='FulFills_FullFillableElements']/Element"></xsl:variable>
		<xsl:variable name="fulfilledSlots" select="referenceList[@name='fulfills']/Element"></xsl:variable>
		<xsl:variable name="workProduct" select="reference[@name='WorkProduct']/Element[@Type='Artifact']"/>
		<xsl:variable name="deliverableParts" select="referenceList[@name='deliverableParts']/Element"/>
		<xsl:variable name="impacts" select="referenceList[@name='impacts']/Element[@Type='WorkProductDescriptor']"/>
		<xsl:variable name="impactedBy" select="referenceList[@name='impactedBy']/Element[@Type='WorkProductDescriptor']"/>
		<xsl:variable name="mandatoryInputTo" select="referenceList[@name='WorkProductDescriptor_MandatoryInputTo_TaskDescriptors']/Element[@Type='TaskDescriptor']"/>
		<xsl:variable name="optionalInputTo" select="referenceList[@name='WorkProductDescriptor_OptionalInputTo_TaskDescriptors']/Element[@Type='TaskDescriptor']"/>
		<xsl:variable name="externalInputTo" select="referenceList[@name='WorkProductDescriptor_ExternalInputTo_TaskDescriptors']/Element[@Type='TaskDescriptor']"/>
		<xsl:variable name="outputFrom" select="referenceList[@name='WorkProductDescriptor_OutputFrom_TaskDescriptors']/Element[@Type='TaskDescriptor']"/>
		<xsl:variable name="mandatoryInputToTaskDescriptors_fromSlots" select="referenceList[@name='mandatoryInputToTaskDescriptors_fromSlots']/Element[@Type='TaskDescriptor']"/>
		<xsl:variable name="optionalInputToTaskDescriptors_fromSlots" select="referenceList[@name='optionalInputToTaskDescriptors_fromSlots']/Element[@Type='TaskDescriptor']"/>
		<xsl:variable name="outputFromTaskDescriptors_fromSlots" select="referenceList[@name='outputFromTaskDescriptors_fromSlots']/Element[@Type='TaskDescriptor']"/>
		<xsl:variable name="superActivities" select="referenceList[@name='superActivities']/Element[@Type='Activity']"/>
		<xsl:variable name="responsibleRole" select="referenceList[@name='WorkProductDescriptor_ResponsibleRoleDescriptors']/Element[@Type='RoleDescriptor']"/>
		<xsl:variable name="workedOnBy" select="referenceList[@name='workedOnBy']/Element"/>

		<xsl:if test="count($fulfillingWorkProducts) + count($fulfilledSlots) + count($responsibleRole) + count($workedOnBy) + count($mandatoryInputTo) + count($optionalInputTo) + count($externalInputTo) + count($outputFrom) + count($mandatoryInputToTaskDescriptors_fromSlots) + count($optionalInputToTaskDescriptors_fromSlots) + count($outputFromTaskDescriptors_fromSlots) + count($impacts) + count($impactedBy)> 0">
			<div class="sectionHeading"><xsl:value-of select="$relationshipsText"/></div>
			<div class="sectionContent">
				<table class="sectionTable" border="0" cellspacing="0" cellpadding="0">
					
					<xsl:if test="count($fulfillingWorkProducts) > 0">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$fulfillingWorkProductsText"/></th>
							<td class="sectionTableCell" colspan="2">
							<ul>
								<xsl:for-each select="$fulfillingWorkProducts">
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
					
					<xsl:if test="count($fulfilledSlots) > 0">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$fulfilledSlotsText"/></th>
							<td class="sectionTableCell" colspan="2">
							<ul>
								<xsl:for-each select="$fulfilledSlots">
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
					
					<xsl:if test="count($impacts) > 0">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$dependentText"/></th>
							<td class="sectionTableCell" colspan="3">
							<ul>
								<xsl:for-each select="$impacts">
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
					<xsl:if test="count($impactedBy) > 0">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$impactingText"/></th>
							<td class="sectionTableCell" colspan="3">
							<ul>
								<xsl:for-each select="$impactedBy">
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
					<xsl:if test="count($responsibleRole) + count($workedOnBy) > 0">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$rolesText"/></th>
							<td class="sectionTableCell" width="30%">
								<span class="sectionTableCellHeading">
									<xsl:value-of select="$responsibleText"/>:
								</span>
								<xsl:if test="count($responsibleRole) > 0">
									<ul>
									<xsl:for-each select="$responsibleRole">
									<xsl:sort select="@DisplayName"/>
										<li>
											<a><xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute><xsl:value-of select="@DisplayName"/></a>
										</li>
									</xsl:for-each>
									</ul>
								</xsl:if>									
							</td>
							<td class="sectionTableCell" colspan="2">
								<span class="sectionTableCellHeading">
									<xsl:value-of select="$modifiedByText"/>:
								</span>
								<xsl:if test="count($workedOnBy) > 0">								
									<ul>
										<xsl:for-each select="$workedOnBy">
										<xsl:sort select="@DisplayName"/>
											<li>
												<a><xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute><xsl:value-of select="@DisplayName"/></a>
											</li>
										</xsl:for-each>
									</ul>
								</xsl:if>										
							</td>
						</tr>
					</xsl:if>
					<xsl:if test="count($mandatoryInputTo) + count($optionalInputTo) + count($externalInputTo) + count($mandatoryInputToTaskDescriptors_fromSlots) + count($optionalInputToTaskDescriptors_fromSlots) > 0">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$inputToText"/></th>
							<td class="sectionTableCell" width="30%">
								<span class="sectionTableCellHeading">
									<xsl:value-of select="$mandatoryText"/>:
								</span>
								<xsl:choose>
									<xsl:when test="count($mandatoryInputTo) + count($mandatoryInputToTaskDescriptors_fromSlots) > 0">
										<ul>
										<xsl:for-each select="$mandatoryInputTo">		
										<xsl:sort select="@DisplayName"/>								
											<li>
												<a><xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute><xsl:value-of select="@DisplayName"/></a>
											</li>
										</xsl:for-each>	
										<xsl:for-each select="$mandatoryInputToTaskDescriptors_fromSlots">		
										<xsl:sort select="@DisplayName"/>								
											<li>
												<a><xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute><xsl:value-of select="@DisplayName"/></a>
											</li>
										</xsl:for-each>							
										</ul>
									</xsl:when>				
									<xsl:otherwise>
									<ul>
										<li>
											<xsl:value-of select="$noneText"/>
										</li>									
									</ul>
									</xsl:otherwise>
								</xsl:choose>
							</td>
							<td class="sectionTableCell" width="30%">
								<span class="sectionTableCellHeading">
									<xsl:value-of select="$optionalText"/>:
								</span>
								<xsl:choose>
									<xsl:when test="count($optionalInputTo) + count($optionalInputToTaskDescriptors_fromSlots) > 0">
									<ul>
										<xsl:for-each select="$optionalInputTo">
										<xsl:sort select="@DisplayName"/>
											<li>
												<a><xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute><xsl:value-of select="@DisplayName"/></a>
											</li>
										</xsl:for-each>	
										<xsl:for-each select="$optionalInputToTaskDescriptors_fromSlots">
										<xsl:sort select="@DisplayName"/>
											<li>
												<a><xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute><xsl:value-of select="@DisplayName"/></a>
											</li>
										</xsl:for-each>							
									</ul>
									</xsl:when>				
									<xsl:otherwise>
									<ul>
										<li>
											<xsl:value-of select="$noneText"/>
										</li>
									</ul>									
									</xsl:otherwise>									
								</xsl:choose>
							</td>
							<td class="sectionTableCell">
								<span class="sectionTableCellHeading">
									<xsl:value-of select="$externalText"/>:
								</span>
								<xsl:choose>
									<xsl:when test="count($externalInputTo) > 0">
									<ul>
										<xsl:for-each select="$externalInputTo">
										<xsl:sort select="@DisplayName"/>
											<li>
												<a><xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute><xsl:value-of select="@DisplayName"/></a>
											</li>
										</xsl:for-each>							
									</ul>
									</xsl:when>				
									<xsl:otherwise>
									<ul>
										<li>
											<xsl:value-of select="$noneText"/>
										</li>
									</ul>									
									</xsl:otherwise>									
								</xsl:choose>
							</td>					
						</tr>
					</xsl:if>					
					<xsl:if test="count($outputFrom)  + count($outputFromTaskDescriptors_fromSlots)> 0">
						<tr valign="top">
							<th class="sectionTableHeading" scope="row"><xsl:value-of select="$outputFromText"/></th>							
							<td class="sectionTableCell" colspan="3">
							<xsl:if test="count($outputFrom) > 0">									
								<ul>
									<xsl:for-each select="$outputFrom">
									<xsl:sort select="@DisplayName"/>
										<li>
											<a><xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute><xsl:value-of select="@DisplayName"/></a>
										</li>
									</xsl:for-each>
								</ul>
							</xsl:if>
							<xsl:if test="count($outputFromTaskDescriptors_fromSlots) > 0">									
								<ul>
									<xsl:for-each select="$outputFromTaskDescriptors_fromSlots">
									<xsl:sort select="@DisplayName"/>
										<li>
											<a><xsl:attribute name="href"><xsl:value-of select="/Element/@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute><xsl:value-of select="@DisplayName"/></a>
										</li>
									</xsl:for-each>
								</ul>
							</xsl:if>
							</td>							
						</tr>
					</xsl:if>
				</table>
			</div>
		</xsl:if>
	</xsl:template>

	<xsl:template name="deliverableSection">
		<xsl:param name="description"/>
		<xsl:variable name="externalDescription" select="$description/attribute[@name='externalDescription']"/>
		<xsl:variable name="deliveredWorkProducts" select="referenceList[@name='deliverableParts']/Element"/>
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
