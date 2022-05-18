package com.example.zad6labv2;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.layout.AnchorPane;


/**
 * Class responsible for constructing the gui of the main scene
 * and adding functionality to its components.
 */
public class MainScene {
    private final AnchorPane root;
    private GetParametersMenu getParametersMenu;

    private static final BooleanProperty runBoardScene = new SimpleBooleanProperty(false);

    MainScene(AnchorPane _root)
    {
        root = _root;
        runBoardScene.addListener(observable -> {
            try {
                boardScene();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });
    }

    /**
     * Method that is run by GetParametersMenu when the parameters have been given
     * and the Construct Grid button has been clicked.
     */
    public static void RunBoardScene()
    {
        runBoardScene.set(true);
    }

    /**
     * Method responsible for constructing the main scene
     */
    public void mainScene()
    {
        getParametersMenu = new GetParametersMenu(root);
        getParametersMenu.constructMenu();
    }

    /**
     * Method responsible for constructing the board scene
     * and setting the functionality of all it's components,
     * most importantly it constructs the grid and starts
     * the simulation.
     */
    public void boardScene() throws InterruptedException {
        root.getChildren().clear();
        int boardWidth = getParametersMenu.getWidth();
        int boardHeight = getParametersMenu.getHeight();
        double probability = getParametersMenu.getProbability();
        int changeSpeed = getParametersMenu.getChangeSpeed();

        ColorBoard colorBoard = new ColorBoard(boardWidth, boardHeight, probability, changeSpeed);
        colorBoard.SetAnchorPaneLayout(true, true, true, true);
        root.getChildren().add(colorBoard);
    }
}
