package com.example.zad6labv2;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.layout.AnchorPane;

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

    public static void RunBoardScene()
    {
        runBoardScene.set(true);
    }

    public void mainScene()
    {
        getParametersMenu = new GetParametersMenu(root);
        getParametersMenu.constructMenu();
        System.out.println("menu size = " + getParametersMenu.getWidth() + ", " + getParametersMenu.getHeight());
    }

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
