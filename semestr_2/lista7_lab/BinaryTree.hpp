#include <iostream>
#include <stack>

template <class T>
class BinaryTree{
public:
    class Node{
    public:
    Node(Node* parent, T value) : parent(parent), data(value){}
        T data;
        Node* parent = nullptr;
        Node* left = nullptr;
        Node* right = nullptr;
    };

    BinaryTree()
    {
        root = nullptr;
    }

    int getCount(){return count;}

    int getDepth(){return _getDepth(root);}
    int _getDepth(Node* node)
    {
        if (node == nullptr)
            return 0;
        else
        {
            int lDepth = _getDepth(node->left);
            int rDepth = _getDepth(node->right);

            if (lDepth > rDepth)
                return (lDepth + 1);
            else
                return (rDepth + 1);
        }
    }

    void insert(T data)
    {
        count++;
        if(root == nullptr)
        {
            root = new Node(nullptr, data);
            return;
        }
        Node* y = nullptr;
        Node* x = root;
        while(x != nullptr)
        {
            y = x;
            if(data < x->data)
                x = x->left;
            else
                x = x->right;
        }
        if(y != nullptr)
        {
            Node* z = new Node(y, data);
            if(data < y->data)
                y->left = z;
            else
                y->right = z;
        }
    }

    Node* search(T k)
    {
        Node* x = root;
        while(x != nullptr)
        {
            if(k == x->data)
                return x;
            if(k < x->data)
                x = x->left;
            else
                x = x->right;
        }
        return nullptr;
    }

    void del(T k)
    {
        Node *x,*y,*z = search(k);
        if(z == nullptr)
            return;
        count--;
        if(z->left == nullptr || z->right == nullptr)
            y = z;
        else
            y = nastepnik(z);
        if(y->left != nullptr)
            x = y->left;
        else
            x = y->right;
        if(x != nullptr)
            x->parent = y->parent;
        if(y->parent == nullptr)
            root = x;
        else if(y == y->parent->left)
            y->parent->left = x;
        else
            y->parent->right = x;
        if(y != z)
            z->data = y->data;
    }

    void draw(Node* node)
    {
        std::cout << node->data << std::endl;
        if(node->left!=nullptr)
            draw(node->left);
        if(node->right!=nullptr)
            draw(node->right);
    }
private:
    Node* nastepnik(Node* x)
    {
        if(x->right != nullptr)
            return nastepnik(x->right);
        Node* y = x->parent;
        while(y != nullptr && x->right == y->right)
        {
            x = y;
            y = y->parent;
        }
        return y;
    }

private:
    int count = 0;
public:
    Node* root = nullptr;
};