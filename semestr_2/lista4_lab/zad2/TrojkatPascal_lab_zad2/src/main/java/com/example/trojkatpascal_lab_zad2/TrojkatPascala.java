package com.example.trojkatpascal_lab_zad2;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.Objects;

public class TrojkatPascala extends Application {
    public static int AppWidth  = 800;
    public static int AppHeight = 800;

    public static Stage mStage;

    @Override
    public void start(Stage stage) throws IOException {
        Parent root = FXMLLoader.load(Objects.requireNonNull(getClass().getResource("mainScene.fxml")));
        mStage = stage;

        Scene mainScene = new Scene(root, AppWidth, AppHeight);
        String css = Objects.requireNonNull(this.getClass().getResource("/style.css")).toExternalForm();
        mainScene.getStylesheets().add(css);

        // set program icon
        Image icon = new Image("icon.png");
        mStage.getIcons().add(icon);
        mStage.setTitle("Trójkąt Pascala");

        mStage.setScene(mainScene);
        mStage.show();
    }

    public static void main(String[] args) {
        launch();
    }
}