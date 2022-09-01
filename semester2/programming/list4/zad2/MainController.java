package com.example.trojkatpascal_lab_zad2;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.paint.Color;

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

    @FXML
    protected void onGenerateButtonClick(ActionEvent event) {
        try {
            numOfRows = Integer.parseInt(numInput.getText());
            triangleOutput.clear();
            triangleOutput.setText(WierszTrojkataPaskala.generateTriangle(numOfRows));
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