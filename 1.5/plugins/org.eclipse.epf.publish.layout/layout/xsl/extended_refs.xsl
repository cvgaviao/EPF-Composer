<?xml version="1.0" encoding="UTF-8"?>
<!-- 
    Copyright (c) 2005, 2012 IBM Corporation and others.
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

	<xsl:include href="qualified_ref.xsl"/>
	<xsl:include href="tables.xsl"/>
	
	<xsl:template name="extendedRefsAllSections">
		<xsl:param name="elementDown"/>
	
		<xsl:for-each select="$elementDown/section[@type='reference']">
			<xsl:for-each select="referenceList[@format='immidate child list']">
				<!--  
				<br/><xsl:value-of select="@referenceName"></xsl:value-of>
				-->
				
				<xsl:call-template name="qualifiedRefField"/>
				<!-- 
				<xsl:variable name="rteText" select="text()"/>
				<xsl:if test="string($rteText) != ''">
				
					<div class="sectionHeading">
						<xsl:value-of select="@name"/>
					</div>
					<div class="sectionContent">
						<table class="sectionTable" border="0" cellspacing="0" cellpadding="0">
							<tr valign="top">
								<td class="sectionTableSingleCell">
									<xsl:value-of disable-output-escaping="yes" select="$rteText"/>
								</td>
							</tr>
						</table>
					</div>

				</xsl:if>
				-->
				 
			</xsl:for-each>
			
		   
			<xsl:call-template name="displayTables">
				<xsl:with-param name="sectionElement" select="."/>
			</xsl:call-template>
			
		</xsl:for-each>
		
	</xsl:template>

</xsl:stylesheet>
