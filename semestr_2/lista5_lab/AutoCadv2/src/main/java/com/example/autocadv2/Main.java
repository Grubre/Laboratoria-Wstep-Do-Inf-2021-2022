package com.example.autocadv2;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.ToolBar;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;

import java.io.IOException;

/**
 * Main class
 */
public class Main extends Application {
    public static int AppWidth  = 800;
    public static int AppHeight = 500;
    private Stage mStage;

    @Override
    public void start(Stage stage) throws IOException {
        mStage = stage;

        AnchorPane root = new AnchorPane();
        MainScene.ConstructScene(root);

        Scene scene = new Scene(root, AppWidth, AppHeight);

        mStage.setTitle("Edytor Ksztaltow");
        mStage.setScene(scene);
        mStage.show();
    }

    public static void main(String[] args) {
        launch();
    }
}