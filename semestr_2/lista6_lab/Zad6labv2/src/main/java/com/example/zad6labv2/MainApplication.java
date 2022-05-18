package com.example.zad6labv2;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.Random;

public class MainApplication extends Application {
    public static Random randomizer = new Random();
    private static Stage mStage;
    @Override
    public void start(Stage stage) throws IOException, InterruptedException {
        mStage = stage;

        AnchorPane root = new AnchorPane();

        Scene scene = new Scene(root, 800, 800);

        MainScene mainScene = new MainScene(root);
        mainScene.mainScene();

        mStage.setTitle("Symulacja kolorow");
        mStage.setScene(scene);
        mStage.show();
    }

    public static Stage getStage()
    {
        return mStage;
    }

    public static void main(String[] args) {
        launch();
    }
}