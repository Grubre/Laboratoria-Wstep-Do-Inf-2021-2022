package com.example.binary_tree_v2;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;

import java.io.IOException;

/**
 * Main function of the client
 */
public class GuiClient extends Application {
    @Override
    public void start(Stage stage) throws IOException {
        AnchorPane root = new AnchorPane();
        Scene scene = new Scene(root, 800, 800);

        MainScene mainScene = new MainScene(stage, root);
        if (mainScene.connect()) {
            mainScene.display();
            stage.setTitle("Binary Tree Displayer Client");
            stage.setScene(scene);
            stage.show();
        }
        else
        {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Failed to connect");
            alert.setHeaderText("Run the server first!");
            alert.setContentText("The port is 3333");

            alert.showAndWait();
        }
    }

    public static void main(String[] args) {
        launch();
    }
}