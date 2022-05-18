package com.example.zad6labv2;

import javafx.scene.Node;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.RowConstraints;
import javafx.scene.paint.Color;

import java.util.concurrent.atomic.AtomicReferenceArray;

/**
 * A container which extends GridPane with added support for multithreaded Cells.
 */
public class ColorBoard extends GridPane {
    private final int gridWidth;
    private final int gridHeight;
    private final double probability;
    private final int changeSpeed;

    /**
     * A variable holding information about current color values
     * in the grid. It is constantly accessed by all the cells
     * and therefore atomic.
     */
    public AtomicReferenceArray<Color> colorsArray;

    ColorBoard(int gridWidth, int gridHeight, double probability, int changeSpeed)
    {
        this.gridWidth = gridWidth;
        this.gridHeight = gridHeight;
        this.probability = probability;
        this.changeSpeed = changeSpeed;

        colorsArray = new AtomicReferenceArray<Color>(gridWidth * gridHeight);

        addRowsAndColumns();

        for(int i = 0; i < gridWidth * gridHeight; i++)
        {
            Cell cell = new Cell(i % gridWidth, i / gridWidth, this);
            Color randColor = getRandomColor();
            cell.setColor(randColor);
            colorsArray.set(i,randColor);
            cell.widthProperty().bind(MainApplication.getStage().widthProperty().divide(gridWidth));
            cell.heightProperty().bind(MainApplication.getStage().heightProperty().divide(gridHeight));
            add(cell, i % gridWidth, i / gridWidth);
        }
        for(int i = 0; i < gridWidth * gridHeight; i++)
        {
            Thread thread = new Thread((Cell)get(i%gridWidth, i/gridWidth));
            thread.setDaemon(true);
            thread.start();
        }

    }
    /**
     * A method which returns average color of the four
     * neighbours of a cell on the grid.
     * @param gridX x position of the cell in the grid
     * @param gridY y position of the cell in the grid
     */
    public Color getNeighbourColors(int gridX, int gridY)
    {
        //System.out.println("Cell at: " + gridX + ", " + gridY + ": ");
        int r = 0;
        int g = 0;
        int b = 0;

        int[][] neighbors = new int[4][2];

        neighbors[0][0] = gridX - 1;
        if(neighbors[0][0] < 0) neighbors[0][0] = gridWidth - 1;
        neighbors[0][1] = gridY;

        neighbors[1][0] = gridX;
        neighbors[1][1] = gridY - 1;
        if(neighbors[1][1] < 0) neighbors[1][1] = gridHeight - 1;

        neighbors[2][0] = gridX + 1;
        if(neighbors[2][0] >= gridWidth) neighbors[2][0] = 0;
        neighbors[2][1] = gridY;

        neighbors[3][0] = gridX;
        neighbors[3][1] = gridY + 1;
        if(neighbors[3][1] >= gridHeight) neighbors[3][1] = 0;

        int neighborsCounted = 0;
        for(int i = 0; i < 4; i++)
        {
            try {
                Cell neighborCell = (Cell) get(neighbors[i][0], neighbors[i][1]);
                if(!neighborCell.isActive())
                    continue;
                Color color = colorsArray.get(neighbors[i][0] + neighbors[i][1] * gridWidth);
                r += (int)( 255 * color.getRed());
                g += (int)( 255 * color.getGreen());
                b += (int)( 255 * color.getBlue());
                neighborsCounted++;
            }catch (Exception e)
            {
                //System.out.println(e.getMessage());
                //System.out.println("Coords of " + i + ": " + neighbors[i][0] + ", " + neighbors[i][1]);
            }
        }
        if(neighborsCounted == 0)
            return colorsArray.get(gridX + gridY * gridWidth);
        r /= neighborsCounted;
        g /= neighborsCounted;
        b /= neighborsCounted;
        return Color.rgb(r,g,b);
    }

    /**
     * A helper method which sets the layout of ColorBoard in the anchor pane.
     * @param Top Boolean which decides whether the ColorBoard should be clipped
     *            to the top in an anchor pane layout.
     * @param Left Boolean which decides whether the ColorBoard should be clipped
     *      *            to the left in an anchor pane layout.
     * @param Bottom Boolean which decides whether the ColorBoard should be clipped
     *      *            to the bottom in an anchor pane layout.
     * @param Right Boolean which decides whether the ColorBoard should be clipped
     *      *            to the right in an anchor pane layout.
     */
    public void SetAnchorPaneLayout(Boolean Top, Boolean Left, Boolean Bottom, Boolean Right)
    {
        if(Top)    AnchorPane.setTopAnchor(this, 0.0);
        if(Left)   AnchorPane.setLeftAnchor(this, 0.0);
        if(Bottom) AnchorPane.setBottomAnchor(this, 0.0);
        if(Right)  AnchorPane.setRightAnchor(this, 0.0);
    }

    /**
     * A method which adds gridWidth amount of columns
     * and gridHeight amount of rows.
     */
    private void addRowsAndColumns()
    {
        for (int i = 0; i < gridWidth; i++) {
            ColumnConstraints colConst = new ColumnConstraints();
            colConst.setFillWidth(true);
            colConst.setPercentWidth(100.0 / gridWidth);
            getColumnConstraints().add(colConst);
        }
        for (int i = 0; i < gridHeight; i++) {
            RowConstraints rowConst = new RowConstraints();
            rowConst.setFillHeight(true);
            rowConst.setPercentHeight(100.0 / gridHeight);
            getRowConstraints().add(rowConst);
        }
    }

    /**
     * @param x x position of the Cell in the grid.
     * @param y y position of the Cell in the grid.
     * @return Cell at position (x,y)
     */
    public Cell get (final int x, final int y) {
        for (Node node : this.getChildren()) {
            if (GridPane.getColumnIndex(node) == x && GridPane.getRowIndex(node) == y) {
                return (Cell)node;
            }
        }
        return null;
    }

    /**
     * @return the probability with which the Cells change their color
     *         value to the average of their four neighbours.
     */
    public double getProbability()
    {
        return probability;
    }
    /**
     * @return the speed with which the Cells change their color
     *         in milliseconds, that value is then randomly multiplied
     *         by a value from interval [0.5,1.5].
     */
    public int getChangeSpeed()
    {
        return changeSpeed;
    }
    /**
     * @return the width of the grid.
     */
    public int getGridWidth(){return gridWidth;}
    /**
     * @return the height of the grid.
     */
    public int getGridHeight(){return gridHeight;}
    /**
     * @return random Color
     */
    public static Color getRandomColor()
    {
        int r = MainApplication.randomizer.nextInt(256);
        int g = MainApplication.randomizer.nextInt(256);
        int b = MainApplication.randomizer.nextInt(256);
        return Color.rgb(r,g,b);
    }
}
