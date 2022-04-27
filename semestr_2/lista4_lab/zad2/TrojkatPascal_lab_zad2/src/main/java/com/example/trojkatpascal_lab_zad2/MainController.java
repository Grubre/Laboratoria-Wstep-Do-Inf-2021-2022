package com.example.trojkatpascal_lab_zad2;

import javafx.beans.property.SimpleIntegerProperty;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.text.Text;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.Window;

import java.io.IOException;
import java.util.Objects;



public class MainController {
    @FXML
    private Button generateButton;
    @FXML
    private TextField numInput;
    @FXML
    private TextArea triangleOutput;

    int numOfRows;

    private void sizeTextAreaToText(TextArea textArea, String text) {
        Text t = new Text(text);
        t.setFont(textArea.getFont());
        StackPane pane = new StackPane(t);
        pane.layout();
        double width = t.getLayoutBounds().getWidth();
        double height = t.getLayoutBounds().getHeight();
        double padding = 150 ;

        Rectangle2D screenBounds = Screen.getPrimary().getBounds();

        TrojkatPascala.mStage.setMinWidth(Math.min((int)(width+padding), screenBounds.getWidth()));
        TrojkatPascala.mStage.setMinHeight(Math.min((int)(height+padding),screenBounds.getHeight()));
        TrojkatPascala.mStage.setMaxWidth(Math.min((int)(width+padding), screenBounds.getWidth()));
        TrojkatPascala.mStage.setMaxHeight(Math.min(Math.max((int)(height+padding),650),screenBounds.getHeight()));
        TrojkatPascala.mStage.centerOnScreen();
        textArea.setMaxWidth(width+padding);
        textArea.setMaxHeight(height+padding);
        textArea.setText(text);
    }

    @FXML
    protected void onGenerateButtonClick(ActionEvent event) {
        try {
            SimpleIntegerProperty count = new SimpleIntegerProperty(20);
            numOfRows = Integer.parseInt(numInput.getText());
            triangleOutput.clear();
            sizeTextAreaToText(triangleOutput, WierszTrojkataPaskala.generateTriangle(numOfRows));
            //triangleOutput.setText(WierszTrojkataPaskala.generateTriangle(numOfRows));
            //count.setValue(triangleOutput.getLayoutBounds().getWidth());



            //triangleOutput.prefWidthProperty().bindBidirectional(count);
            //triangleOutput.minWidthProperty().bindBidirectional(count);
            //TrojkatPascala.mStage.sizeToScene();
            //System.out.println(triangleOutput.getLayoutBounds());
            //TrojkatPascala.mStage.setMinWidth(triangleOutput.getMinWidth());
            //TrojkatPascala.mStage.setMinHeight(triangleOutput.maxHeight(-1));
        }
        catch(Exception e){
            triangleOutput.setText("Zle wprowadzone dane!\n");
        }
    }

    @FXML
    protected void onGenerateButtonHover() {

    }

    @FXML
    protected void onGenerateButtonHoverExit() {

    }
}