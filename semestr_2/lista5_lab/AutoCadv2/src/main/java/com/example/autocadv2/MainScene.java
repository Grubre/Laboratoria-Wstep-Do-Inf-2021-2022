package com.example.autocadv2;

import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Polygon;
import javafx.scene.shape.Rectangle;
import javafx.scene.shape.Shape;

/**
 * Class responsible for creating and adding functionality
 * to all of the objects.
 */
public class MainScene {
    /**
     * Enum holding the program state, on button press when (state == RECTANGLE/CIRCLE/TRIANGLE)
     * you create a new shape and when (state == MODIFY) you modify existing shapes
     */
    public enum State {
        RECTANGLE, CIRCLE, TRIANGLE, MODIFY
    }
    private static State state = State.MODIFY;

    /**
     * Size of the sidebar containing all the buttons (in pixels)
     */
    public static int sidebarSize = 150;

    private static Pane canvasPane;
    /**
     * Object holding the currently selected shape
     */
    private static Shape selectedShape = null;

    private static boolean isShapeBeingSelected = false;

    private static final ShapeCreator shapeCreator = new ShapeCreator();

    private static ColorPicker colorPicker = new ColorPicker();

    /**
     * Method that calls addComponents() to construct the GUI.
     * It is also responsible for creating and setting the properties of all the shapes.
     */
    public static void ConstructScene(AnchorPane root) {
        addComponents(root);
        canvasPane.setOnMouseClicked(new EventHandler<MouseEvent>() {
            @Override
            public void handle(MouseEvent event) {
                // COLOR PICKER
                if (selectedShape != null && event.getButton().equals(MouseButton.SECONDARY)) {
                    canvasPane.getChildren().remove(colorPicker);
                    colorPicker = new ColorPicker((Color) selectedShape.getFill());
                    colorPicker.setTranslateX(event.getSceneX() - MainScene.sidebarSize);
                    colorPicker.setTranslateY(event.getSceneY());
                    canvasPane.getChildren().add(colorPicker);
                    colorPicker.setOnAction(new EventHandler() {
                        public void handle(Event t) {
                            selectedShape.setFill(colorPicker.getValue());
                        }
                    });
                }
                if (event.getButton().equals(MouseButton.PRIMARY)) {
                    canvasPane.getChildren().remove(colorPicker);
                    int m_x = (int) (event.getSceneX() - sidebarSize);
                    int m_y = (int) (event.getSceneY());
                    if (state == State.TRIANGLE) {
                        shapeCreator.add(canvasPane, m_x, m_y);
                        if (shapeCreator.getPointsPlaced() < 3)
                            return;
                        Polygon triangle = shapeCreator.getTriangle();
                        canvasPane.getChildren().add(triangle);
                        shapeCreator.reset(canvasPane);
                        addSelectionProperty(triangle);
                        triangle.setOnScroll(new EventHandler<ScrollEvent>() {
                            @Override
                            public void handle(ScrollEvent scrollEvent) {
                                if (triangle != selectedShape)
                                    return;
                                double resizeValue = Math.signum(scrollEvent.getDeltaY());
                                triangle.setScaleX(triangle.getScaleX() + 0.1 * resizeValue);
                                triangle.setScaleY(triangle.getScaleY() + 0.1 * resizeValue);
                                triangle.setScaleZ(triangle.getScaleZ() + 0.1 * resizeValue);
                            }
                        });
                    } else if (state == State.CIRCLE) {
                        shapeCreator.add(canvasPane, m_x, m_y);
                        if (shapeCreator.getPointsPlaced() < 2)
                            return;
                        Circle circle = shapeCreator.getCircle();
                        canvasPane.getChildren().add(circle);
                        shapeCreator.reset(canvasPane);
                        addSelectionProperty(circle);
                        circle.setOnScroll(new EventHandler<ScrollEvent>() {
                            @Override
                            public void handle(ScrollEvent scrollEvent) {
                                if (circle != selectedShape)
                                    return;
                                double resizeValue = Math.signum(scrollEvent.getDeltaY());
                                circle.setRadius(circle.getRadius() + 10 * resizeValue);
                            }
                        });
                    } else if (state == State.RECTANGLE) {
                        shapeCreator.add(canvasPane, m_x, m_y);
                        if (shapeCreator.getPointsPlaced() < 2) {
                            return;
                        }
                        Rectangle rectangle = shapeCreator.getRectangle();
                        shapeCreator.reset(canvasPane);
                        canvasPane.getChildren().add(rectangle);
                        addSelectionProperty(rectangle);
                        rectangle.setOnScroll(new EventHandler<ScrollEvent>() {
                            @Override
                            public void handle(ScrollEvent scrollEvent) {
                                if (rectangle != selectedShape)
                                    return;
                                double resizeValue = Math.signum(scrollEvent.getDeltaY());
                                rectangle.setWidth(rectangle.getWidth() + 10 * resizeValue);
                                rectangle.setHeight(rectangle.getHeight() + 10 * resizeValue);
                            }
                        });
                    } else if (state == State.MODIFY) {
                        if (!isShapeBeingSelected && selectedShape != null) {
                            selectedShape.setStrokeWidth(0);
                            selectedShape = null;
                        }
                    }
                    isShapeBeingSelected = false;
                }

            }
        });
    }

    /**
     * Method that adds GUI components to the scene and configures them.
     */
    public static void addComponents(AnchorPane root) {
        // ADDING THE SIDEBAR
        AnchorPane sidebar = new AnchorPane();
        sidebar.setStyle("-fx-background-color: gray");
        AnchorPane.setTopAnchor(sidebar, 0.0);
        AnchorPane.setLeftAnchor(sidebar, 0.0);
        AnchorPane.setBottomAnchor(sidebar, 0.0);
        sidebar.setPrefWidth(sidebarSize);
        root.getChildren().add(sidebar);

        // ADDING VBOX AND BUTTONS TO THE SIDEBAR
        VBox sidebarVbox = new VBox();
        AnchorPane.setTopAnchor(sidebarVbox, 40.0);
        sidebarVbox.setPrefWidth(sidebarSize);
        sidebarVbox.setAlignment(Pos.CENTER);
        Button addCircleButton = new Button();
        addCircleButton.setText("Kolko");
        addCircleButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent e) {
                state = State.CIRCLE;
                deselect();
                shapeCreator.reset(canvasPane);
            }
        });
        Button addRectangleButton = new Button();
        addRectangleButton.setText("Prostokat");
        addRectangleButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent e) {
                state = State.RECTANGLE;
                deselect();
                shapeCreator.reset(canvasPane);
            }
        });
        Button addTriangleButton = new Button();
        addTriangleButton.setText("Trojkat");
        addTriangleButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent e) {
                state = State.TRIANGLE;
                deselect();
                shapeCreator.reset(canvasPane);
            }
        });
        Button modifyButton = new Button();
        modifyButton.setText("Modyfikuj");
        modifyButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent e) {
                state = State.MODIFY;
                deselect();
                shapeCreator.reset(canvasPane);
            }
        });
        sidebarVbox.getChildren().add(addCircleButton);
        sidebarVbox.getChildren().add(addRectangleButton);
        sidebarVbox.getChildren().add(addTriangleButton);
        sidebarVbox.getChildren().add(modifyButton);
        sidebar.getChildren().add(sidebarVbox);

        // ADDING THE CANVAS
        canvasPane = new Pane();
        AnchorPane.setTopAnchor(canvasPane, 0.0);
        AnchorPane.setLeftAnchor(canvasPane, (double) sidebarSize);
        AnchorPane.setBottomAnchor(canvasPane, 0.0);
        AnchorPane.setRightAnchor(canvasPane, 0.0);
        root.getChildren().add(canvasPane);
        sidebar.toFront();

        // ADDING THE TOOLBAR
        ToolBar toolBar = new ToolBar();
        toolBar.setStyle("-fx-alignment: center");
        root.getChildren().add(toolBar);
        toolBar.setPrefWidth(MainScene.sidebarSize);
        AnchorPane.setLeftAnchor(toolBar, (double) 0.0);
        AnchorPane.setTopAnchor(toolBar, 0.0);
        Button infoButton = new Button();
        infoButton.setText("Info");
        infoButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent actionEvent) {
                Alert alert = new Alert(Alert.AlertType.INFORMATION);
                alert.setTitle("Informacje");
                alert.setHeaderText(null);
                alert.setContentText("Nazwa: Edytor Ksztaltow\nAutor: Jakub Ogrodowczyk" +
                        "\nPrzeznaczenie: Tworzenie i modyfikowanie ksztaltow");

                alert.showAndWait();
            }
        });
        Button helpButton = new Button();
        helpButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent actionEvent) {
                Alert alert = new Alert(Alert.AlertType.INFORMATION);
                alert.setTitle("Instrukcja");
                alert.setHeaderText(null);
                alert.setContentText("Aby stworzyc ksztalt wybierz jeden z przyciskow po lewej:\n" +
                        "- [Kolko] - 2 punkty wyznaczajace srodek i promien,\n- [Prostokat] - 2 punkty wyznaczajace przeciwlegle rogi" +
                        ",\n- [Trojkat] - 3 punkty wyznaczajace wierzcholki,\nAby ksztalt modyfikowac wcisnij przycisk [Modyfikuj]," +
                        " a nastepnie wybierz jeden z istniejacych juz ksztaltow." +
                        " Aby:\n- Przemiescic ksztalt - zlap go myszka i przesun,\n- Zmienic rozmiar ksztaltu" +
                        " - uzyj scrolla\n- Zmienic kolor - nacisnij prawy przycisk myszy" +
                        " i wybierz kolor z menu\n- Obrocic - przytrzymaj scroll i przesuwaj myszka w prawo lub lewo");

                alert.showAndWait();
            }
        });
        helpButton.setText("Instrukcja");
        toolBar.getItems().add(infoButton);
        toolBar.getItems().add(helpButton);

    }

    private static void deselect() {
        if(selectedShape !=null)
        {
            selectedShape.setStrokeWidth(0);
            selectedShape = null;
        }
    }
    /**
     * Method that adds a mouse event that makes the object selectable.
     * The selected object is stored in the selectedShape variable.
     * @param target Shape that is meant to have the drag and rotate properties added.
     */
    public static void addSelectionProperty(Shape target)
    {
        target.setOnMousePressed(
                new EventHandler<MouseEvent>(){
                    public void handle(MouseEvent e){
                        if(isShapeBeingSelected || state != State.MODIFY)
                            return;
                        if(selectedShape != null)
                            selectedShape.setStrokeWidth(0);
                        target.toFront();
                        target.setStroke(Color.YELLOW);
                        target.setStrokeWidth(5);
                        selectedShape = target;
                        isShapeBeingSelected = true;
                    }
                });
    }
    public static State getState()
    {
        return state;
    }
}
