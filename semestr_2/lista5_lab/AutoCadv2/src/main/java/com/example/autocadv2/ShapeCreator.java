package com.example.autocadv2;

import javafx.geometry.Point2D;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Polygon;
import javafx.scene.shape.Rectangle;

import java.util.ArrayList;
import java.util.List;
/**
 * Class that creates shapes from a list of points
 */
public class ShapeCreator {
    /**
     * Amount of currently placed points
     */
    private int pointsPlaced;
    /**
     * List holding current points
     */
    private List<Point2D> shapePoints;
    /**
     * List holding the circles that are displayed on the Pane
     */
    private List<Circle> renderedPoints;
    ShapeCreator()
    {
        pointsPlaced = 0;
        shapePoints = new ArrayList<>();
        renderedPoints = new ArrayList<>();
    }
    /**
     * Adds a Point2D to the shapePoints list and a circle to the renderedPoints list,
     * it then adds the circle to canvasPane children so that they are displayed.
     * @param canvasPane Pane that the circles will be displayed on
     * @param m_x,m_y the coordinates of the point that is being added
     */
    void add(Pane canvasPane, int m_x, int m_y)
    {
        // The circle is only for visualisation purposes
        Circle circle = new Circle();
        circle.setRadius(5);
        circle.setFill(Color.BLACK);
        circle.setStrokeWidth(2);
        circle.setStroke(Color.YELLOW);
        circle.setCenterX(m_x);
        circle.setCenterY(m_y);
        circle.toFront();
        canvasPane.getChildren().add(circle);

        // Here we create the actual point
        Point2D point = new Point2D(m_x,m_y);
        pointsPlaced++;
        shapePoints.add(point);
        renderedPoints.add(circle);
    }
    /**
     * pointsPlaced getter
     * @return amount of currently placed points
     */
    int getPointsPlaced()
    {
        return pointsPlaced;
    }
    /**
     * Creates a circle and adds the dragging and rotation property.
     * @return circle constructed from the two points specified in shapePoints
     */
    Circle getCircle()
    {
        if(pointsPlaced != 2)
            return null;
        Circle circle = new Circle();
        double radius = shapePoints.get(0).distance(shapePoints.get(1));
        circle.setRadius(radius);
        circle.setCenterX(shapePoints.get(0).getX());
        circle.setCenterY(shapePoints.get(0).getY());
        PropertyAdder propertyAdder = new PropertyAdder(circle);
        return circle;
    }
    /**
     * Creates a triangle and adds the dragging and rotation property.
     * @return triangle constructed from the three points specified in shapePoints
     */
    Polygon getTriangle()
    {
        if(pointsPlaced != 3)
            return null;
        Polygon triangle = new Polygon(
                shapePoints.get(0).getX(), shapePoints.get(0).getY(),
                shapePoints.get(1).getX(), shapePoints.get(1).getY(),
                shapePoints.get(2).getX(), shapePoints.get(2).getY()
        );
        PropertyAdder propertyAdder = new PropertyAdder(triangle);
        triangle.setFill(Color.BLACK);
        return triangle;
    }
    /**
     * Creates a rectangle and adds the dragging and rotation property.
     * @return rectangle constructed from the two points specified in shapePoints
     */
    Rectangle getRectangle()
    {
        if(pointsPlaced != 2)
            return null;
        Rectangle rectangle = new Rectangle(
                Math.min(shapePoints.get(0).getX(), shapePoints.get(1).getX()),
                Math.min(shapePoints.get(0).getY(), shapePoints.get(1).getY()),
                Math.abs(shapePoints.get(0).getX() - shapePoints.get(1).getX()),
                Math.abs(shapePoints.get(0).getY() - shapePoints.get(1).getY()));
        PropertyAdder propertyAdder = new PropertyAdder(rectangle);
        rectangle.setFill(Color.BLACK);
        return rectangle;
    }
    /**
     * Resets the points list and removes the visible points from the root
     * @param canvasPane Pane from which the circles are to be removed
     */
    void reset(Pane canvasPane)
    {
        for(int i = 0; i < pointsPlaced; i++)
        {
            canvasPane.getChildren().remove(renderedPoints.get(i));
        }
        pointsPlaced = 0;
        renderedPoints.clear();
        shapePoints.clear();
    }
}
