package com.example.trojkatpascal_lab_zad2;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class WierszTrojkataPaskala {
    public static String generateTriangle(int n) throws IllegalArgumentException, IOException {
        StringBuilder triangle = new StringBuilder();

        if(n<=0)
            throw new IllegalArgumentException("");

            Process getTriangle = Runtime.getRuntime().exec("trojkat.exe " + Integer.toString(n));

            BufferedReader reader = new BufferedReader(new InputStreamReader(getTriangle.getInputStream()));

            String line;
            while((line = reader.readLine()) != null)
            {
                triangle.append(line);
                triangle.append("\n");
            }

        return triangle.toString();
    }

}