import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.Random;

/**
 * Javafx thread main class
 */
public class MainApplication extends Application {
    /**
     * Global static Random class instance
     */
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

    /**
     * @return the main stage, needed for calculating size of gui components
     */
    public static Stage getStage()
    {
        return mStage;
    }

    public static void main(String[] args) {
        launch();
    }
}