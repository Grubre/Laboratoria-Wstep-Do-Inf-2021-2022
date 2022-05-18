package com.example.zad6labv2;

import javafx.application.Platform;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;

public class Cell extends Rectangle implements Runnable {
    private final int gridX;
    private final int gridY;

    private Color color;

    private final ColorBoard parent;

    private boolean run = true;
    private boolean active = true;

    Cell(int gridX, int gridY, ColorBoard parent)
    {
        this.gridX = gridX;
        this.gridY = gridY;
        this.parent = parent;

        System.out.println("Creating a new cell at: " + gridX + ", " + gridY);

        setOnMouseClicked(mouseEvent -> {
            synchronized (this)
            {
                active = !active;
                if(active)
                    notify();
            }
        });
    }

    @Override
    public void run() {
        while(run)
        {
            try{
                if(!active)
                    synchronized (this)
                    {
                        while(!active) wait();
                    }
                double changeSpeedRandFactor = Math.max(1,MainApplication.randomizer.nextDouble(1.0) + 0.5);
                Thread.sleep((long)(changeSpeedRandFactor * parent.getChangeSpeed()));

                if(MainApplication.randomizer.nextDouble(1) <= (1 - parent.getProbability()))
                {
                    color = parent.getNeighbourColors(gridX, gridY);
                }
                else
                {
                    color = ColorBoard.getRandomColor();
                }
                parent.colorsArray.set(gridX + gridY * parent.getGridWidth(), color);
                Platform.runLater(new Runnable() {
                    @Override
                    public void run() {
                        setFill(color);
                    }
                });
            }catch (InterruptedException e) {e.printStackTrace();}
            Thread.yield();
        }
    }

    public Color getColor(){return color;}
    public int getGridX(){return gridX;}
    public int getGridY(){return gridY;}

    public void setColor(Color _color){color = _color; setFill(color);}
    public void setActive(boolean active){this.active=active;}
    public Boolean isActive(){return active;}
    public void off(){this.run=false;}
}
