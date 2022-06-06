package com.example.binary_tree_v2;

import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

/**
 * Class responsible for displaying the GUI and interaction with the user
 */
public class MainScene {
    public static Stage stage;
    private AnchorPane root;

    private Socket socket = null;
    private ObjectOutputStream output = null;
    private ObjectInputStream input  = null;

    /**
     * Constructor of the MainScene class
     * @param stage given by the Main class
     * @param root container that is also given by the main class
     */
    public MainScene(Stage stage, AnchorPane root)
    {
        this.stage = stage;
        this.root  = root;
    }

    /**
     * Function that tries to connect with the server.
     * The whole program shuts down if it fails.
     * @return whether we managed to connect to the server
     */
    public boolean connect()
    {
        System.out.println("Trying to connect...");
        try{
            socket = new Socket("127.0.0.1", 3333);
            if(!socket.isConnected())
                throw new Exception();
            output = new ObjectOutputStream(socket.getOutputStream());
            input  = new ObjectInputStream(socket.getInputStream());
            System.out.println("Succesfully connected to the server");
        }
        catch(Exception e)
        {
            System.out.println("Failed to connect to the server...");
            return false;
        }
        return true;
    }

    /**
     * Function responsible for creating the GUI and displaying it.
     */
    public void display()
    {
        BorderPane screen = new BorderPane();
        AnchorPane.setTopAnchor(screen, 0.0);
        AnchorPane.setLeftAnchor(screen, 0.0);
        AnchorPane.setBottomAnchor(screen, 0.0);
        AnchorPane.setRightAnchor(screen, 0.0);
        screen.setStyle("-fx-background-color: red");

        HBox topBar = new HBox();
        topBar.prefHeightProperty().bind(screen.heightProperty().divide(10));
        topBar.setAlignment(Pos.CENTER);
        topBar.setStyle("-fx-background-color: #E8D3B9");
        screen.setTop(topBar);

        TextField textInput = new TextField();

        TreeDisplayer treeDisplayer = new TreeDisplayer();
        AnchorPane.setTopAnchor(screen, 0.0);
        AnchorPane.setLeftAnchor(screen, 0.0);
        AnchorPane.setBottomAnchor(screen, 0.0);
        AnchorPane.setRightAnchor(screen, 0.0);
        treeDisplayer.setStyle("-fx-background-color: white");
        screen.setCenter(treeDisplayer);

        Button sendButton = new Button("Send");

        sendButton.setOnAction(actionEvent -> {
            String textinput,command;
            Connection.type type;
            try{
                textinput = textInput.getText().trim().replaceAll(" +", " ").toLowerCase();
                String[] splitInput = textinput.split(" ",3);
                command = splitInput[0];
                if(splitInput.length < 2)
                {
                    throw new Exception("Not enough arguments!");
                }
                type = Connection.type.valueOf(splitInput[1].toUpperCase());
                output.writeObject(type);
                switch(command)
                {
                    case "draw"->{
                        if(splitInput.length > 2)
                        {
                            throw new Exception("Too many arguments for the draw command!");
                        }
                        switch (type)
                        {
                            case STRING -> {
                                output.writeObject(new Command<String>("draw", ""));
                                BinaryTree<String> tree = (BinaryTree<String>) input.readObject();
                                treeDisplayer.displayTree(tree);
                            }
                            case DOUBLE -> {
                                output.writeObject(new Command<Double>("draw", 0.0));
                                BinaryTree<Double> tree = (BinaryTree<Double>) input.readObject();
                                treeDisplayer.displayTree(tree);
                            }
                            case INT    -> {
                                output.writeObject(new Command<Integer>("draw", 0));
                                BinaryTree<Integer> tree = (BinaryTree<Integer>) input.readObject();
                                treeDisplayer.displayTree(tree);
                            }
                        }
                    }
                    case "insert"->{
                        if(splitInput.length < 3)
                        {
                            throw new Exception("Not enough arguments for the insert command!");
                        }
                        switch (type)
                        {
                            case STRING -> {
                                String textoutput = splitInput[2];
                                output.writeObject(new Command<String>("insert", textoutput));
                            }
                            case DOUBLE -> {
                                Double doubleoutput = Double.parseDouble(splitInput[2]);
                                output.writeObject(new Command<Double>("insert", doubleoutput));
                            }
                            case INT    -> {
                                System.out.println("\""+splitInput[2]+"\"");
                                Integer integeroutput = Integer.parseInt(splitInput[2]);
                                output.writeObject(new Command<Integer>("insert", integeroutput));
                            }
                        }
                        System.out.println("Inserted a new value");
                        treeDisplayer.displayString("Inserted a new value");
                    }
                    case "delete"->{
                        if(splitInput.length < 3)
                        {
                            throw new Exception("Not enough arguments for the delete command!");
                        }
                        switch (type)
                        {
                            case STRING -> {
                                String textoutput = splitInput[2];
                                output.writeObject(new Command<String>("delete", textoutput));
                            }
                            case DOUBLE -> {
                                Double doubleoutput = Double.parseDouble(splitInput[2]);
                                output.writeObject(new Command<Double>("delete", doubleoutput));
                            }
                            case INT    -> {
                                Integer integeroutput = Integer.parseInt(splitInput[2]);
                                output.writeObject(new Command<Integer>("delete", integeroutput));
                            }
                        }
                        System.out.println("Deleted a value");
                        treeDisplayer.displayString("Deleted a value");
                    }
                    case "search"->{
                        if(splitInput.length < 3)
                        {
                            throw new Exception("Not enough arguments for the search command!");
                        }
                        switch (type)
                        {
                            case STRING -> {
                                String textoutput = splitInput[2];
                                output.writeObject(new Command<String>("search", textoutput));
                            }
                            case DOUBLE -> {
                                Double doubleoutput = Double.parseDouble(splitInput[2]);
                                output.writeObject(new Command<Double>("search", doubleoutput));
                            }
                            case INT    -> {
                                Integer integeroutput = Integer.parseInt(splitInput[2]);
                                output.writeObject(new Command<Integer>("search", integeroutput));
                            }
                        }
                        treeDisplayer.displayString((String) input.readObject());
                    }
                    default -> {
                        throw new Exception("No such command!");
                    }
                }
            }
            catch(IllegalArgumentException e)
            {
                System.out.println("Wrong type - use String, Double or Int!");
                treeDisplayer.displayString("Wrong type, use String, Double or Int!");
            }
            catch(Exception e)
            {
                System.out.println(e.getMessage());
                treeDisplayer.displayString(e.getMessage());
            }
        });

        topBar.getChildren().addAll(textInput,sendButton);
        root.getChildren().add(screen);
    }

}
