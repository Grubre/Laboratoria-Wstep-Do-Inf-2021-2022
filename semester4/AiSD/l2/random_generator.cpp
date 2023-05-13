#include <cstdlib>
#include <iostream>
#include <random>

int main(int argc, char** argv) {
    if(argc != 2) {
        std::cerr << "You must provide size" << std::endl;
        return -1;
    }
    unsigned int n = std::stoi(argv[1]);


    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distrib(0, 2 * n - 1);
    while(n-->0) {
        std::cout << distrib(gen) << std::endl;
    }
    return 0;
}
