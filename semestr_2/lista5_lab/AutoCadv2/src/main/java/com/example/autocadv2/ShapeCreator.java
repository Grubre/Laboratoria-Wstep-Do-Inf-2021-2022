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
    private int pointsPlaced;
    private List<Point2D> shapePoints;
    private List<Circle> renderedPoints;
    ShapeCreator()
    {
        pointsPlaced = 0;
        shapePoints = new ArrayList<>();
        renderedPoints = new ArrayList<>();
    }
    /**
     * Adds a point at (m_x,m_y)
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
     */
    int getPointsPlaced()
    {
        return pointsPlaced;
    }
    /**
     * Constructs a circle from two points
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
     * Constructs a triangle from three points
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
     * Constructs a triangle from two points
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
