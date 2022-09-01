import javafx.beans.binding.DoubleBinding;
import javafx.scene.control.Label;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.scene.shape.Line;
import javafx.scene.text.Font;
import javafx.scene.text.Text;

import java.io.Serializable;

/**
 * Class responsible for displaying trees or error feedback when something goes wrong.
 */
public class TreeDisplayer extends StackPane {
    /**
     * Method which displays a give binary search tree
     * @param tree Tree which is supposed to be displayed.
     * @param <T> Type of the tree held in the tree
     */
    public <T extends Comparable & Serializable> void displayTree(BinaryTree<T> tree)
    {
        getChildren().clear();
        if(tree.getCount()==0)
        {
            Text text = new Text("Tree is empty");
            text.setFont(Font.font(25));
            getChildren().add(text);
            return;
        }
        tree.draw();

        Pane pane = new Pane();
        getChildren().add(pane);
        pane.setStyle("-fx-background-color: white");

        recursiveDisplay(pane, 0, MainScene.stage.widthProperty().divide(2),tree.root);
    }

    /**
     * Private method responsible for recursively displaying nodes
     * @param pane container in which the tree will be displayed
     * @param depth of the node in the tree
     * @param layoutX x position of the node in the pane
     * @param node to be displayed
     * @param <T> type of the data held by the node
     */
    private <T extends Comparable & Serializable> void recursiveDisplay(Pane pane, double depth, DoubleBinding layoutX , BinaryTree<T>.TreeNode<T> node)
    {
        Label label = new Label(node.data.toString());
        label.layoutXProperty().bind(layoutX.subtract(label.widthProperty().divide(2)));
        double yDistance = 100;
        label.setLayoutY(depth * yDistance);
        label.setStyle("-fx-background-color: coral; -fx-padding: 10px;");
        if(node.left!=null)
        {
            Line line = new Line();
            line.startXProperty().bind(layoutX);
            line.setStartY(depth * yDistance + 10);
            line.endXProperty().bind(layoutX.subtract(MainScene.stage.widthProperty().divide(Math.pow(2,depth + 2))));
            line.setEndY((depth + 1) * yDistance);
            pane.getChildren().add(line);
            recursiveDisplay(pane, depth + 1, layoutX.subtract(MainScene.stage.widthProperty().divide(Math.pow(2,depth + 2))) ,node.left);
        }
        if(node.right!=null)
        {
            Line line = new Line();
            line.startXProperty().bind(layoutX);
            line.setStartY(depth * yDistance + 10);
            line.endXProperty().bind(layoutX.add(MainScene.stage.widthProperty().divide(Math.pow(2,depth + 2))));
            line.setEndY((depth + 1) * yDistance);
            pane.getChildren().add(line);
            recursiveDisplay(pane, depth + 1, layoutX.add(MainScene.stage.widthProperty().divide(Math.pow(2,depth + 2))) ,node.right);
        }
        pane.getChildren().add(label);
        System.out.println("WIELKOSC = "+label.getWidth());
    }

    /**
     * Function that displays a string where normally the tree would be displayed
     * @param t String to be displayed
     */
    public void displayString(String t)
    {
        Text text = new Text(t);
        text.setFont(Font.font(25));
        getChildren().clear();
        getChildren().add(text);
    }
}
