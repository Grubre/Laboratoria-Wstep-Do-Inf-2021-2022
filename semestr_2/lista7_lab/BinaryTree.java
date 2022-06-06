import java.io.Serializable;
import java.util.LinkedList;

/**
 * Implementation of binary search tree
 * @param <T> type of data held in the nodes
 */
public class BinaryTree<T extends Comparable & Serializable> implements Serializable {
    /**
     * Class holding value, parent and reference to the left and right child of a given node
     * @param <T> type of data held in the node
     */
    public class TreeNode<T extends Comparable & Serializable> implements Serializable
    {
        public T data;
        public TreeNode<T> parent = null;
        public TreeNode<T> left = null;
        public TreeNode<T> right = null;

        public TreeNode(T data, TreeNode<T> parent)
        {
            this.data = data;
            this.parent = parent;
        }
    }
    public TreeNode<T> root;

    private int count = 0;

    /**
     * @return count of nodes in the tree
     */
    public int getCount(){return count;}

    /**
     * @return depth of the tree
     */
    public int getDepth(){return _getDepth(root);}

    /**
     * private implementation of recursive get depth algorithm
     * @param node node on which we start the algorithm
     * @return depth of the tree at root == node
     */
    private int _getDepth(TreeNode node)
    {
        if (node == null)
            return 0;
        else
        {
            int lDepth = _getDepth(node.left);
            int rDepth = _getDepth(node.right);

            if (lDepth > rDepth)
                return (lDepth + 1);
            else
                return (rDepth + 1);
        }
    }
    public BinaryTree()
    {
        root = null;
    }

    /**
     * Inserts a new node with value data to the tree
     * @param data value which will be held by the new node
     */
    public void insert(T data)
    {
        count++;
        if(root == null)
        {
            root = new TreeNode<T>(data, null);
            return;
        }
        TreeNode y = null;
        TreeNode x = root;
        while(x != null)
        {
            y = x;
            if(data.compareTo(x.data) < 0)
                x = x.left;
            else
                x = x.right;
        }
        if(y != null)
        {
            TreeNode z = new TreeNode(data, y);
            if(data.compareTo(y.data) < 0)
                y.left = z;
            else
                y.right = z;
        }
    }

    /**
     * Looks for a given value k in the tree
     * @param k value to look for
     * @return whether we found the value or not
     */
    public TreeNode search(T k)
    {
        TreeNode x = root;
        while(x != null)
        {
            if(k.equals(x.data))
                return x;
            if(k.compareTo(x.data) < 0)
                x = x.left;
            else
                x = x.right;
        }
        return null;
    }

    /**
     * Prints the tree to the console
     */
    public void draw()
    {
        if(root == null)
        {
            System.out.println("Tree is empty");
            return;
        }
        LinkedList<TreeNode> stack = new LinkedList<TreeNode>();

        stack.push(root);
        System.out.println("Printing tree:");

        while(stack.size()!=0)
        {
            TreeNode top = stack.pop();
            if(top.parent != null)
            {
                System.out.println("(val="+top.data+", parent="+top.parent.hashCode()+", parent val="+top.parent.data+")");
            }
            else
                System.out.println("(val="+top.data+", parent=null)");
            if(top.left != null) {
                stack.push(top.left);
            }
            if(top.right != null)
            {
                stack.push(top.right);
            }
        }
    }

    /**
     * Deletes node which holds value k
     * @param k value of the node to be deleted
     */
    public void delete(T k)
    {
        TreeNode<T> x,y,z = search(k);
        if(z == null)
            return;
        count--;
        if(z.left == null || z.right == null)
            y = z;
        else
            y = nastepnik(z);
        if(y.left != null)
            x = y.left;
        else
            x = y.right;
        if(x != null)
            x.parent = y.parent;
        if(y.parent == null)
            root = x;
        else if(y == y.parent.left)
            y.parent.left = x;
        else
            y.parent.right = x;
        if(y != z)
            z.data = y.data;
    }

    /**
     * Finds the successor of a given node
     * @param x Node whose successor we are looking for
     * @return successor of node x
     */
    private TreeNode nastepnik(TreeNode x)
    {
        if(x.right != null)
            return nastepnik(x.right);
        TreeNode y = x.parent;
        while(y != null && x.right == y.right)
        {
            x = y;
            y = y.parent;
        }
        return y;
    }
}
