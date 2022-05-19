package com.example.zad6labv2;

import javafx.application.Platform;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;

/**
 * Class responsible for handling the simulation of a single Cell,
 * it overrides the run method which is run as a separate thread
 * as well as holds all the functionality needed to change it's color
 * and deactivate itself with accordance to the simulation specification.
 */
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

    /**
     * Function that is run as a separate thread.
     * Changes the Color of the Cell, waits for [0.5 * changeSpeed, 1.5 * changeSpeed] milliseconds.
     */
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
                boolean atomicallySet = false;
                while(!atomicallySet)
                {
                    atomicallySet = parent.colorsArray.compareAndSet(gridX + gridY * parent.getGridWidth(), parent.colorsArray.get(gridX + gridY * parent.getGridWidth()), color);
                }

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

    /**
     * @return Color value of the Cell
     */
    public Color getColor(){return color;}
    /**
     * @return x position in the grid
     */
    public int getGridX(){return gridX;}
    /**
     * @return y position in the grid
     */
    public int getGridY(){return gridY;}

    /**
     * @param _color new Color value of the Cell
     */
    public void setColor(Color _color){color = _color; setFill(color);}

    /**
     * @param active changes the state of the Cell
     */
    public void setActive(boolean active){this.active=active;}

    /**
     * @return whether the cell is active
     */
    public Boolean isActive(){return active;}

    /**
     * Turns off the Thread component of the Cell.
     */
    public void off(){this.run=false;}
}
