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

	<xsl:template name="customOppositeRelationshipsEmbedded">
		<xsl:param name="elementDown"/>
		<xsl:param name="iconLevel"/>
		<xsl:param name="layoutLocation"/>
		
		<xsl:for-each select="referenceList[@referenceType='customOpposite' and @layout=$layoutLocation]">
			
					<xsl:if test="count(*) > 0">
						<tr valign="top">

							<th class="sectionTableHeading" scope="row">
								<xsl:value-of select="@name"/>
							</th>
							<td class="sectionTableCell" colspan="2">
								<xsl:for-each select="*">
									<ul>
										<xsl:if test="name()='Element'">
											<li>
												<img>
													<xsl:choose> 
														<xsl:when test="$iconLevel = 'two'"> 
															<xsl:attribute name="src">./../../<xsl:value-of select="@ShapeiconUrl"/></xsl:attribute>
														</xsl:when> 
														<xsl:otherwise> 
															<xsl:attribute name="src">./../../../<xsl:value-of select="@ShapeiconUrl"/></xsl:attribute>
														</xsl:otherwise>
													</xsl:choose> 				
													<xsl:attribute name="height">16</xsl:attribute>
													<xsl:attribute name="width">16</xsl:attribute>
												</img>
												<a>
													<xsl:attribute name="href"><xsl:value-of select="@BackPath"/><xsl:value-of select="@Url"/></xsl:attribute>
													<xsl:value-of disable-output-escaping="yes" select="@Name"/>
												</a>
											</li>
										</xsl:if>
									</ul>
								</xsl:for-each>
							</td>
						</tr>
					</xsl:if>	
				 
		</xsl:for-each>
		
	</xsl:template>

</xsl:stylesheet>