package com.example.binary_tree_v2;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.net.Socket;

/**
 * Class running as a separate thread, responsible for communication with the client
 * and operations on the tree for the given client.
 */
public class Connection implements Runnable {
    BinaryTree<String> stringTree;
    BinaryTree<Double> doubleTree;
    BinaryTree<Integer> intTree;

    /**
     * Enum sent from the client to the server when running a command.
     * Determines on which tree the operation should be performed.
     */
    public enum type implements Serializable {
        STRING,
        DOUBLE,
        INT
    }

    Socket socket;

    Connection(Socket socket)
    {
        this.socket = socket;
        stringTree = new BinaryTree< String>();
        doubleTree = new BinaryTree< Double>();
        intTree    = new BinaryTree<Integer>();
    }

    /**
     * Main loop running on a separate thread.
     * Is responsible for handling the input and output with the client.
     */
    @Override
    public void run() {
        try {
            ObjectOutputStream output = new ObjectOutputStream(socket.getOutputStream());
            ObjectInputStream input  = new ObjectInputStream(socket.getInputStream());
            while(true)
            {
                type treeType = (type) input.readObject();
                String serverResponse;
                switch(treeType)
                {
                    case STRING -> {
                        Command<String> command = (Command<String>) input.readObject();
                        switch(command.command())
                        {
                            case "insert" -> {
                                stringTree.insert(command.value());
                            }
                            case "delete" -> {
                                stringTree.delete(command.value());
                            }
                            case "search" -> {
                                if(stringTree.search(command.value())!=null)
                                    serverResponse = "Value found!";
                                else
                                    serverResponse = "Value not found!";
                                output.writeObject(serverResponse);
                            }
                            case "draw" -> {
                                output.writeObject(stringTree);
                                stringTree.draw();
                                output.reset();
                            }
                            default -> {
                                System.out.println("No such command");
                            }
                        }
                    }
                    case DOUBLE -> {
                        Command<Double> command = (Command<Double>) input.readObject();
                        switch(command.command())
                        {
                            case "insert" -> {
                                doubleTree.insert(command.value());
                            }
                            case "delete" -> {
                                doubleTree.delete(command.value());
                            }
                            case "search" -> {
                                if(doubleTree.search(command.value())!=null)
                                    serverResponse = "Value found!";
                                else
                                    serverResponse = "Value not found!";
                                output.writeObject(serverResponse);
                            }
                            case "draw" -> {
                                output.writeObject(doubleTree);
                                doubleTree.draw();
                                output.reset();
                            }
                            default -> {
                                System.out.println("No such command");
                            }
                        }
                    }
                    case INT -> {
                        Command<Integer> command = (Command<Integer>) input.readObject();
                        switch(command.command())
                        {
                            case "insert" -> {
                                intTree.insert(command.value());
                            }
                            case "delete" -> {
                                intTree.delete(command.value());
                            }
                            case "search" -> {
                                if(intTree.search(command.value())!=null)
                                    serverResponse = "Value found!";
                                else
                                    serverResponse = "Value not found!";
                                output.writeObject(serverResponse);
                            }
                            case "draw" -> {
                                output.writeObject(intTree);
                                intTree.draw();
                                output.reset();
                            }
                            default -> {
                                System.out.println("No such command");
                            }
                        }
                    }
                }
                output.reset();
            }
        } catch (IOException e) {
            try {
                System.out.println("Client disconnected!");
                socket.close();
                return;
            } catch (IOException ex) {
                ex.printStackTrace();
            }

            e.printStackTrace();
        }
        catch(ClassNotFoundException d)
        {
            System.out.println("Input error");
        }
    }
}
