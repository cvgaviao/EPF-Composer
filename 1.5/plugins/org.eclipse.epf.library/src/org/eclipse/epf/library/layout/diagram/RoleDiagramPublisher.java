//------------------------------------------------------------------------------
// Copyright (c) 2005, 2006 IBM Corporation and others.
// All rights reserved. This program and the accompanying materials
// are made available under the terms of the Eclipse Public License v1.0
// which accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
// IBM Corporation - initial implementation
//------------------------------------------------------------------------------
package org.eclipse.epf.library.layout.diagram;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.TexturePaint;
import java.awt.geom.Line2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Iterator;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.JPanel;

import org.eclipse.core.runtime.Platform;
import org.eclipse.epf.common.utils.I18nUtil;
import org.eclipse.epf.common.utils.ImageUtil;
import org.eclipse.epf.library.LibraryPlugin;
import org.eclipse.epf.library.LibraryResources;
import org.eclipse.epf.library.layout.IElementLayout;
import org.eclipse.epf.library.layout.elements.RoleLayout;
import org.eclipse.epf.library.preferences.LibraryPreferences;
import org.eclipse.epf.uma.Role;
import org.eclipse.epf.uma.Task;
import org.eclipse.epf.uma.WorkProduct;
import org.eclipse.swt.SWT;

/**
 * Renders the Role diagram using Java2D and saves it as a JPEG file.
 * 
 * @author Kelvin Low
 * @author Jinhua Xi
 * @since 1.0
 */
public class RoleDiagramPublisher {

	private static final String DEFAULT_FONT_NAME = "Arial"; //$NON-NLS-1$

	private static final int DEFAULT_FONT_SIZE = 10;

	private static final int DEFAULT_X_MARGIN = 200;

	// The maximum width of the generated image.
	private int maxImageWidth = 1024;

	// The maximum height of the generated image.
	private int maxImageHeight = 2048;

	// The border width.
	private int borderWidth = 10;

	// The border height.
	private int borderHeight = 10;

	// The additional spacing before the left-most Task and/or Work Product
	// image.
	private int xMargin = 240;

	// The horizontal spacing between the element images.
	private int xSpacing;

	// The vertical spacing between the element images.
	private int ySpacing;

	// The maximun number of text lines for the element labels.
	private int maxTextLines;

	private int linePadding = 12;

	private Font textFont = null;

	/**
	 * Creates a new <code>ImagePublisher</code>.
	 */
	public RoleDiagramPublisher() {
		// Select the font for rendering the element names.
		String fontName;
		int fontSize;
		fontName = getFontName();
		if (SWT.getPlatform().equals(Platform.WS_WIN32)) {
			fontSize = I18nUtil.getInt(
					LibraryResources.roleDiagramFont_size_win32,
					DEFAULT_FONT_SIZE);
		} else {
			fontSize = I18nUtil.getInt(LibraryResources.roleDiagramFont_size,
					DEFAULT_FONT_SIZE);
		}
		if (fontName.startsWith("[")) { //$NON-NLS-1$
			fontName = DEFAULT_FONT_NAME;
		}
		textFont = new Font(fontName, Font.PLAIN, fontSize);

		xMargin = I18nUtil.getInt(LibraryResources.roleDiagram_xMargin,
				DEFAULT_X_MARGIN);

		xSpacing = LibraryPreferences.getRoleDiagramHorizontalSpacing();
		ySpacing = LibraryPreferences.getRoleDiagramVerticalSpacing();
		maxTextLines = LibraryPreferences.getRoleDiagramMaximumTextLines();
	}
	
	private String getFontName() {
		String fontName;
		if (Platform.getNL().startsWith("ja")) { //$NON-NLS-1$
			if (SWT.getPlatform().equals(Platform.WS_WIN32)) {
				fontName = LibraryResources.roleDiagramFont_name_win32_ja;
			} else {
				fontName = LibraryResources.roleDiagramFont_name_ja;
			}
		} else if (Platform.getNL().startsWith("ko")) { //$NON-NLS-1$
			if (SWT.getPlatform().equals(Platform.WS_WIN32)) {
				fontName = LibraryResources.roleDiagramFont_name_win32_ko;
			} else {
				fontName = LibraryResources.roleDiagramFont_name_ko;
			}
		} else if (Platform.getNL().startsWith("zh_TW")) { //$NON-NLS-1$
			if (SWT.getPlatform().equals(Platform.WS_WIN32)) {
				fontName = LibraryResources.roleDiagramFont_name_win32_zh_TW;
			} else {
				fontName = LibraryResources.roleDiagramFont_name_zh_TW;
			}
		} else if (Platform.getNL().startsWith("zh")) { //$NON-NLS-1$
			if (SWT.getPlatform().equals(Platform.WS_WIN32)) {
				fontName = LibraryResources.roleDiagramFont_name_win32_zh;
			} else {
				fontName = LibraryResources.roleDiagramFont_name_zh;
			}
		} else {
			if (SWT.getPlatform().equals(Platform.WS_WIN32)) {
				fontName = LibraryResources.roleDiagramFont_name_win32;
			} else {
				fontName = LibraryResources.roleDiagramFont_name;
			}
		}
		return fontName;

	}

	/**
	 * Generates the Role Diagram HTML source for the given role.
	 * 
	 * @param role
	 *            The Role element.
	 * @param path
	 *            The publish path.
	 * @return A <code>MethodElementDiagram</code> object associated with the
	 *         published diagram.
	 */
	public MethodElementDiagram publish(RoleLayout roleLayout, File path) {

		Role role = (Role) roleLayout.getElement();

		if (role == null) {
			throw new IllegalArgumentException();
		}

		try {
			MethodElementDiagram elementDiagram = new MethodElementDiagram(role);

			String publishDir = roleLayout.getLayoutMgr().getPublishDir();
			String mapName = roleLayout.getDisplayName();
			if (mapName == null || mapName.length() == 0) {
				mapName = "Unknown"; //$NON-NLS-1$
			} else {
				mapName = mapName.replace(' ', '_');
			}
			HTMLMap htmlMap = new HTMLMap(mapName);
			elementDiagram.setHTMLMap(htmlMap);

			// Create the AWT JPanel for rendering the diagram.
			JPanel panel = new JPanel();

			// Create the output BufferedImage.
			BufferedImage outputImage = new BufferedImage(maxImageWidth,
					maxImageHeight, BufferedImage.TYPE_INT_RGB);
			Graphics2D g2d = (Graphics2D) outputImage.getGraphics();

			// Switch on anti-aliasing.
			g2d.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
					RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
			g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
					RenderingHints.VALUE_ANTIALIAS_ON);
			g2d.fillRect(0, 0, maxImageWidth, maxImageHeight);

			// Set the stroke for drawing the association lines.
			g2d.setStroke(new BasicStroke(1.25f));
			g2d.setColor(Color.BLACK);

			int xOffset = xSpacing / 2;

			// Renders the Tasks performed by the Role.
			List tasks = roleLayout.getPerforms();
			int totalTasks = tasks != null ? tasks.size() : 0;
			Point taskStartPoint = new Point(borderWidth + xMargin,
					borderHeight);
			Point taskEndPoint = new Point(borderWidth + xMargin, borderHeight);
			int x = taskStartPoint.x;
			int y = taskStartPoint.y;
			int tasksEndX = 0;
			if (totalTasks > 0) {
				int taskNo = 1;
				for (Iterator i = tasks.iterator(); i.hasNext(); taskNo++) {
					Task task = (Task) i.next();

					IElementLayout taskLayout = roleLayout.getLayoutMgr()
							.getLayout(task, true);

					String taskName = taskLayout.getDisplayName();
					String imageFile = publishDir
							+ taskLayout.getDiagramiconUrl();
					try {
						BufferedImage taskImage = ImageUtil.getBufferedImage(
								imageFile, panel);
						int taskWidth = taskImage.getWidth();
						int taskHeight = taskImage.getHeight();
						if (taskNo == 1) {
							taskEndPoint.x += taskWidth;
							taskEndPoint.y += taskHeight;
							tasksEndX = taskEndPoint.x;
						}
						Rectangle taskImageRect = new Rectangle(x, y,
								taskWidth, taskHeight);
						TexturePaint taskPaint = new TexturePaint(taskImage,
								taskImageRect);
						g2d.setPaint(taskPaint);
						g2d.fill(taskImageRect);
						if (taskName != null) {
							ImageUtil.drawText(g2d, taskName, textFont,
									Color.BLACK, x - xOffset, taskEndPoint.y,
									taskWidth + xSpacing, maxTextLines);
						}

						String href = taskLayout.getFilePath(roleLayout)
								+ taskLayout.getFileName(".html"); //$NON-NLS-1$
						HTMLArea htmlArea = new HTMLArea(task.getGuid(), href,
								"rect", taskImageRect, taskName); //$NON-NLS-1$
						htmlMap.addArea(htmlArea);

						if (x + taskWidth > tasksEndX) {
							tasksEndX = x + taskWidth;
						}

						if (i.hasNext()) {
							int xIncrement = taskWidth + xSpacing;
							x += xIncrement;
							taskEndPoint.x += xIncrement;
							if ((x + xIncrement + xOffset + borderWidth) > maxImageWidth) {
								x = taskStartPoint.x;
								int yIncrement = taskHeight + ySpacing;
								y += yIncrement;
								taskEndPoint.y += yIncrement;
								taskEndPoint.x = taskStartPoint.x + taskWidth;
							}
						}
					} catch (Exception e) {
						LibraryPlugin.getDefault().getLogger().logError(e);
					}
				}
			}

			// Renders the Work Products that the Role is responsible for.
			List workProducts = roleLayout.getResponsibleFor();
			int totalWorkProducts = workProducts != null ? workProducts.size()
					: 0;
			Point workProductStartPoint = new Point(taskStartPoint.x,
					borderHeight);
			Point workProductEndPoint = new Point(taskStartPoint.x,
					borderHeight);
			if (totalTasks > 0 && totalWorkProducts > 0) {
				workProductStartPoint.y = taskEndPoint.y + ySpacing + ySpacing
						/ 2;
				workProductEndPoint.y = workProductStartPoint.y;
			} else if (totalTasks > 0) {
				workProductEndPoint.y = taskEndPoint.y + borderHeight;
			}
			x = workProductStartPoint.x;
			y = workProductStartPoint.y;
			int workProductsEndX = 0;
			if (totalWorkProducts > 0) {
				int workProductNo = 1;
				for (Iterator i = workProducts.iterator(); i.hasNext(); workProductNo++) {
					WorkProduct workProduct = (WorkProduct) i.next();

					IElementLayout wpLayout = roleLayout.getLayoutMgr()
							.getLayout(workProduct, true);

					String workProductName = wpLayout.getDisplayName();
					String imageFile = publishDir
							+ wpLayout.getDiagramiconUrl();
					try {
						BufferedImage workProductImage = ImageUtil
								.getBufferedImage(imageFile, panel);
						int workProductWidth = workProductImage.getWidth();
						int workProductHeight = workProductImage.getHeight();
						if (workProductNo == 1) {
							workProductEndPoint.x += workProductWidth;
							workProductEndPoint.y += workProductHeight;
							workProductsEndX = workProductEndPoint.x;
						}
						Rectangle workProductImageRect = new Rectangle(x, y,
								workProductWidth, workProductHeight);
						TexturePaint workProductPaint = new TexturePaint(
								workProductImage, workProductImageRect);
						g2d.setPaint(workProductPaint);
						g2d.fill(workProductImageRect);
						if (workProductName != null) {
							ImageUtil.drawText(g2d, workProductName, textFont,
									Color.BLACK, x - xOffset,
									workProductEndPoint.y, workProductWidth
											+ xSpacing, maxTextLines);
						}

						String href = wpLayout.getFilePath(roleLayout)
								+ wpLayout.getFileName(".html"); //$NON-NLS-1$

						HTMLArea htmlArea = new HTMLArea(workProduct.getGuid(),
								href,
								"rect", workProductImageRect, workProductName); //$NON-NLS-1$
						htmlMap.addArea(htmlArea);

						if (x + workProductWidth > workProductsEndX) {
							workProductsEndX = x + workProductWidth;
						}

						if (i.hasNext()) {
							int xIncrement = workProductWidth + xSpacing;
							x += xIncrement;
							workProductEndPoint.x += xIncrement;
							if ((x + xIncrement + xOffset + borderWidth) > maxImageWidth) {
								x = workProductStartPoint.x;
								int yIncrement = workProductHeight + ySpacing;
								y += yIncrement;
								workProductEndPoint.y += yIncrement;
								workProductEndPoint.x = workProductStartPoint.x
										+ workProductWidth;
							}
						}
					} catch (Exception e) {
						LibraryPlugin.getDefault().getLogger().logError(e);
					}
				}
			}

			// Render the Role.
			Point roleStartPoint = new Point(xOffset - 5, borderHeight);
			Point roleEndPoint = roleStartPoint;
			try {
				String roleName = roleLayout.getDisplayName();
				String imageFile = publishDir + roleLayout.getDiagramiconUrl();
				BufferedImage roleImage = ImageUtil.getBufferedImage(imageFile,
						panel);
				int roleWidth = roleImage.getWidth();
				int roleHeight = roleImage.getHeight();
				x = xOffset - 5;
				y = borderHeight;
				roleEndPoint.x += roleWidth;
				roleEndPoint.y += roleHeight;
				if (totalWorkProducts > 0) {
					y = borderHeight
							+ (workProductEndPoint.y - taskStartPoint.y - roleHeight)
							/ 2;

					// Render the "responsible for" association line.
					Point startPoint = new Point(x + roleWidth + linePadding
							/ 2, y + roleHeight / 2);
					int lineEndPoint = workProductStartPoint.x - linePadding;
					if (lineEndPoint > (DEFAULT_X_MARGIN - xSpacing / 2)) {
						lineEndPoint = (DEFAULT_X_MARGIN - xSpacing / 2);
					}
					Point endPoint = new Point(workProductStartPoint.x
							- linePadding, workProductStartPoint.y
							+ (workProductEndPoint.y - workProductStartPoint.y)
							/ 2);
					renderAssociation(g2d, startPoint, endPoint,
							LibraryResources.roleDiagramResponsibleFor_text,
							textFont, Color.BLACK);
				} else if (totalTasks > 0) {
					y = borderHeight
							+ (taskEndPoint.y - taskStartPoint.y - roleHeight)
							/ 2;
				}
				roleStartPoint.y = y;
				roleEndPoint.y = y + roleHeight;

				if (totalTasks > 0) {
					// Render the "performs" association line.
					Point startPoint = new Point(x + roleWidth + linePadding
							/ 2, y + roleHeight / 2);
					int lineEndPoint = taskStartPoint.x - linePadding;
					if (lineEndPoint > (DEFAULT_X_MARGIN - xSpacing / 2)) {
						lineEndPoint = (DEFAULT_X_MARGIN - xSpacing / 2);
					}
					Point endPoint = new Point(lineEndPoint, borderHeight
							+ (taskEndPoint.y - taskStartPoint.y) / 2);
					renderAssociation(g2d, startPoint, endPoint,
							LibraryResources.roleDiagramPerforms_text,
							textFont, Color.BLACK);
				}
				Rectangle roleImageRect = new Rectangle(x, y, roleWidth,
						roleHeight);
				TexturePaint rolePaint = new TexturePaint(roleImage,
						roleImageRect);
				g2d.setPaint(rolePaint);
				g2d.fill(roleImageRect);
				if (roleName != null) {
					ImageUtil.drawText(g2d, roleName, textFont, Color.BLACK, x
							- xOffset, y + roleHeight, roleWidth + xSpacing,
							maxTextLines);
				}
			} catch (Exception e) {
				LibraryPlugin.getDefault().getLogger().logError(e);
			}

			// Calculate the optimal width and height for the rendered image.
			int optimalWidth = Math.max(tasksEndX, workProductsEndX)
					+ borderWidth;
			optimalWidth = Math.max(optimalWidth, roleEndPoint.x + borderWidth);

			optimalWidth = Math.min(optimalWidth, maxImageWidth);

			int optimalHeight = workProductEndPoint.y + borderHeight;
			optimalHeight = Math.max(optimalHeight, roleEndPoint.y
					+ borderHeight);
			optimalWidth += xOffset;
			optimalHeight += (maxTextLines * 10);

			optimalHeight = Math.min(optimalHeight, maxImageHeight);
			BufferedImage optimalImage = outputImage.getSubimage(0, 0,
					optimalWidth, optimalHeight);

			// Save the image as a JPEG file.
			File jpgFile = new File(path, roleLayout.getFilePath()
					+ roleLayout.getFileName(".jpg")); //$NON-NLS-1$
			File parentFolder = jpgFile.getParentFile();
			if (!parentFolder.exists()) {
				parentFolder.mkdirs();
			}
			ImageIO.write(optimalImage, "jpg", jpgFile); //$NON-NLS-1$
			elementDiagram.setImageFileName(jpgFile.getName());
			return elementDiagram;
		} catch (Exception e) {
			LibraryPlugin.getDefault().getLogger().logError(e);
			return null;
		}
	}

	/**
	 * Renders the association line.
	 * 
	 * @param g2d
	 *            The Graphics2D context.
	 * @param startPoint
	 *            The start point of the line.
	 * @param endPoint
	 *            The end point of the line.
	 * @param name
	 *            The association name.
	 * @param font
	 *            The font for rendering the association name.
	 * @param color
	 *            the color for rendering the association name.
	 */
	protected void renderAssociation(Graphics2D g2d, Point startPoint,
			Point endPoint, String name, Font font, Color color) {
		g2d.draw(new Line2D.Double(startPoint.x, startPoint.y, endPoint.x,
				endPoint.y));
		int textX = startPoint.x + (endPoint.x - startPoint.x) / 2 - 40;
		int textY = startPoint.y + (endPoint.y - startPoint.y) / 2 - 5;
		if (startPoint.y != endPoint.y) {
			g2d.setColor(Color.WHITE);
			g2d.fillRect(textX, textY, 80, 10);
		} else {
			textY -= 10;
		}
		ImageUtil.drawText(g2d, name, font, color, textX, textY, 80, 3);
	}

}
