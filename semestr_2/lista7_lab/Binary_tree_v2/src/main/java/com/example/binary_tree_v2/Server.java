package com.example.binary_tree_v2;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;

public class Server {
    public static void main(String[] args) {
        ServerSocket server = null;
        try {
            server = new ServerSocket(3333);
            server.setReuseAddress(true);

            System.out.println("Started server!");
            while (true) {
                try{
                    Socket client = server.accept();
                    System.out.println("New client connected!");
                    Connection clientSock = new Connection(client);
                    Thread thread = new Thread(clientSock);
                    thread.setDaemon(true);
                    thread.start();
                }catch(Exception e){
                    e.printStackTrace();
                }

            }
        }catch(Exception e){
            e.printStackTrace();
        }
    }
}
