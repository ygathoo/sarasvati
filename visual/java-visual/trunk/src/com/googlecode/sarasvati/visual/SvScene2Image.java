/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common
 * Development and Distribution License("CDDL") (collectively, the
 * "License"). You may not use this file except in compliance with the
 * License. You can obtain a copy of the License at
 * http://www.netbeans.org/cddl-gplv2.html
 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the
 * specific language governing permissions and limitations under the
 * License.  When distributing the software, include this License Header
 * Notice in each file and include the License file at
 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the GPL Version 2 section of the License file that
 * accompanied this code. If applicable, add the following below the
 * License Header, with the fields enclosed by brackets [] replaced by
 * your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * If you wish your version of this file to be governed by only the CDDL
 * or only the GPL Version 2, indicate your decision by adding
 * "[Contributor] elects to include this software in this distribution
 * under the [CDDL or GPL Version 2] license." If you do not indicate a
 * single choice of license, a recipient has the option to distribute
 * your version of this file under either the CDDL, the GPL Version 2 or
 * to extend the choice of license to its licensees as provided above.
 * However, if you add GPL Version 2 code and therefore, elected the GPL
 * Version 2 license, then the option applies only if the new code is
 * made subject to such option by the copyright holder.
 *
 * Contributor(s):
 *
 * Portions Copyrighted 2008 Sun Microsystems, Inc.
 */
package com.googlecode.sarasvati.visual;

import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import org.netbeans.api.visual.export.WidgetPolygonalCoordinates;
import org.netbeans.api.visual.widget.ConnectionWidget;
import org.netbeans.api.visual.widget.Scene;
import org.netbeans.api.visual.widget.Widget;

/**
 * Implementation class for SceneExporter. The SceneExporter calls either the export method
 * with an ImageEncoding parameter to set the file export type or it calls
 * getSceneImageMapCoordinates.
 * @author krichard
 */
public class SvScene2Image
{
    protected Scene scene;
    protected double scale = 1;

    /**
     * Creates an instance of a Scene2Image object.
     * @param scene the Scene to be exported as an image.
     * @param file the file to which the image is to be saved. There is no extension
     * check done on the file name. Meaning that a "png" image may be saved with a "txt"
     * extension.
     */
    public SvScene2Image(Scene scene)
    {
        this.scene = scene;
    }

    /**
     * Calculates the coordinates of individual polygons which fully encompass each
     * Widget. These coordinates can be used to create HTML image maps. Note that
     * a call to getSceneImageMapCoordinates will first export the Scene in the established
     * format.
     * @param margin The size of the clickable margin around each widget. This is
     * meant for defining the sensitivity of the links around the connection objects.
     * @return list of WidgetPolygonalCoordinates. The WidgetPolygonalCoordinates
     * is simply a class to hold the Widget and Polygon together.
     */
    public ArrayList<WidgetPolygonalCoordinates> getSceneImageMapCoordinates(int margin) {

        ArrayList<WidgetPolygonalCoordinates> list = new ArrayList<WidgetPolygonalCoordinates>();
        List<Widget> layers = scene.getChildren();

        boolean oneLayer = false; //in case select widget option is true. This is a flag
        // to only pass through one layer.

        for (Widget layer : layers) {

            if (oneLayer) {
                break;
            }

            List<Widget> widgets = layer.getChildren();

            for (Widget w : widgets) {
                Polygon polygon = new Polygon();

                //if it is a connection widget, then get the control points
                //and build the polygon to surround the connection.
                if (w instanceof ConnectionWidget) {
                    List<Point> controlPoints = ((ConnectionWidget) w).getControlPoints();
                    int numPoints = controlPoints.size();
                    if (numPoints == 0) {
                        continue;
                    }

                    Point start = controlPoints.get(0);
                    Point finish = controlPoints.get(controlPoints.size() - 1);

                    Point currentPoint = start;

                    int rise = 1, run = 1;
                    //start by creating a boundary along the bottom or right.
                    //note that the last point uses the slope calculated in
                    //the previous iteration.
                    for (int i = 0; i < numPoints; i++) {
                        int x;
                        int y;
                        Point nextPoint = null;

                        if (i + 1 < numPoints) {
                            nextPoint = controlPoints.get(i + 1);

                            rise = currentPoint.y - nextPoint.y;
                            run = currentPoint.x - nextPoint.x;
                        }

                        x = currentPoint.x;
                        y = currentPoint.y;

                        int xMargin = 0;
                        int yMargin = 0;

                        if (rise == 0) //connector segment is horizontal
                        {
                            yMargin = margin;
                        } else if (run == 0)//connector segment is vertical
                        {
                            xMargin = margin;
                        } else {
                            int sign = (rise * run) / Math.abs(rise * run); // either 1 or -1

                            xMargin = sign * margin;
                            yMargin = margin;
                        }

                        polygon.addPoint((int) (x * scale + xMargin), (int) (y * scale + yMargin));

                        currentPoint = nextPoint;
                    }

                    currentPoint = finish;

                    //come back around the top or left
                    for (int i = numPoints - 1; i >= 0; i--) {

                        int x;
                        int y;

                        Point prevPoint = null;

                        if (i - 1 >= 0) {
                            prevPoint = controlPoints.get(i - 1);
                            rise = currentPoint.y - prevPoint.y;
                            run = currentPoint.x - prevPoint.x;
                        }

                        x = currentPoint.x;
                        y = currentPoint.y;

                        int xMargin = 0;
                        int yMargin = 0;

                        if (rise == 0) //connector segment is horizontal
                        {
                            yMargin = -margin;
                        } else if (run == 0)//connector segment is vertical
                        {
                            xMargin = -margin;
                        } else {
                            int sign = (rise * run) / Math.abs(rise * run); // either 1 or -1

                            xMargin = sign * margin;
                            yMargin = -margin;
                        }

                        polygon.addPoint((int) (x * scale + xMargin), (int) (y * scale + yMargin));

                        currentPoint = prevPoint;
                    }

                } else {
                    Point p0 = w.getLocation();

                    Rectangle r = w.getPreferredBounds();
                    int width = (int) (r.width * scale);
                    int height = (int) (r.height * scale);
                    int x = (int) (p0.x * scale);
                    int y = (int) (p0.y * scale);

                    polygon.addPoint(x - 1, y - 1);
                    polygon.addPoint(x + width + 1, y - 1);
                    polygon.addPoint(x + width + 1, y + height + 1);
                    polygon.addPoint(x - 1, y + height + 1);
                }

                list.add(new WidgetPolygonalCoordinates(w, polygon));

            }

        }

        return list;
    }
}
