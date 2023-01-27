#include <iostream>
#include <vector>
#include <memory>


class Node
{
public:
    Node()
    {
        neighbours.reserve(50);
    }

    void add_neighbour(std::shared_ptr<Node> node) {
        neighbours.push_back(std::shared_ptr<Node>(node));
    }

    auto get_neighbours()
    {
        return neighbours;
    }
private:
    std::vector<std::shared_ptr<Node>> neighbours;
};


std::vector<std::shared_ptr<Node>> generate_clique(int n)
{
    std::vector<std::shared_ptr<Node>> nodes(n, std::make_shared<Node>());
    for(int i = 0; i < n; i++)
    {
        for(int j = 0; j < i; j++)
        {
            std::cout << i << ", " << j << std::endl;
            nodes[i]->add_neighbour(nodes[j]);
        }
    }
    return nodes;
}



int main()
{
    constexpr int n = 100;
    auto nodes = generate_clique(n);
    return 0;
}






