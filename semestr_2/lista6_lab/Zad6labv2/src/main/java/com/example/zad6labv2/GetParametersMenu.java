package com.example.zad6labv2;


import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;

/**
 * A class responsible for constructing the menu and extracting
 * parameters needed for the simulation.
 */
public class GetParametersMenu {
    private int width;
    private int height;
    private int speed;
    private double probability;

    private final Pane root;

    private final VBox menu;
    private TextField numInput;
    private Text text;

    public GetParametersMenu(Pane root) {
        this.root = root;
        menu = new VBox();
    }

    /**
     * Constructs all the gui components of the menu.
     */
    public void constructMenu()
    {
        menu.setAlignment(Pos.CENTER);
        menu.setPadding(new Insets(0,100,100,100));
        makeMenuResponsive();
        root.getChildren().add(menu);


        text = new Text("Pass arguments as: \"width height speed probability\"\nex. 15 15 5 0.5 ");
        text.setTextAlignment(TextAlignment.CENTER);
        numInput = ConstructNumInput();
        Button getInputButton = ConstructGetInputButton();
        menu.getChildren().addAll(text, numInput, getInputButton);
    }

    /**
     * Constructs the getInputButton component.
     */
    private Button ConstructGetInputButton()
    {
        Button getInputButton = new Button();
        getInputButton.setOnAction(actionEvent -> {
            String input = numInput.getText();
            try {
                int _width = Integer.parseInt(input.split(" ")[0]);
                int _height = Integer.parseInt(input.split(" ")[1]);
                int _speed = Integer.parseInt(input.split(" ")[2]);
                double _probability = Double.parseDouble(input.split(" ")[3]);
                //System.out.println(_width + ", " + _height + ", " + _speed + ", " + _probability);
                if (_width <= 0 || _height <= 0) {
                    text.setText("Wymiary musza byc dodatnie!");
                    throw new IllegalArgumentException("Wymiary musza byc dodatnie!");
                }
                if (_speed <= 0) {
                    text.setText("Szybkosc zmiany koloru musi byc dodatnia!");
                    throw new IllegalArgumentException("Szybkosc zmiany koloru musi byc dodatnia!");
                }
                if (_probability < 0 || _probability > 1) {
                    text.setText("Prawdopodobienstwo zmiany koloru musi byc dodatnie!");
                    throw new IllegalArgumentException("Prawdopodobienstwo zmiany koloru musi byc dodatnie!");
                }
                width = _width;
                height = _height;
                speed = _speed;
                probability = _probability;
                MainScene.RunBoardScene();
            } catch (Exception e) {
                text.setText("Parametry musza byc liczbami!");
                System.out.println(e.getMessage());
            }
        });

        getInputButton.setText("Construct grid");
        return getInputButton;
    }

    /**
     * Constructs the numInput component.
     */
    private TextField ConstructNumInput()
    {
        TextField numInput = new TextField();
        numInput.setAlignment(Pos.CENTER);
        numInput.setMinWidth(300);
        numInput.setPrefWidth(300);
        numInput.setMaxHeight(300);
        return numInput;
    }

    /**
     * Sets the layout in an anchor pane.
     */
    private void makeMenuResponsive()
    {
        AnchorPane.setTopAnchor(menu, 0.0);
        AnchorPane.setLeftAnchor(menu, 100.0);
        AnchorPane.setBottomAnchor(menu, 0.0);
        AnchorPane.setRightAnchor(menu, 100.0);
    }

    /**
     * @return the simulation grid width
     */
    public int getWidth()
    {
        return width;
    }
    /**
     * @return the simulation grid height
     */
    public int getHeight()
    {
        return height;
    }
    /**
     * @return the simulation color change speed
     */
    public int getChangeSpeed()
    {
        return speed;
    }
    /**
     * @return the simulation get average neighbor color probability
     */
    public double getProbability()
    {
        return probability;
    }
}
