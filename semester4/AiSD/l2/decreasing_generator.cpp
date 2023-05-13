#include <cstdlib>
#include <iostream>
#include <random>
#include <vector>
#include <algorithm>

int main(int argc, char** argv) {
    if(argc != 2) {
        std::cerr << "You must provide size" << std::endl;
        return -1;
    }
    unsigned int n = std::stoi(argv[1]);

    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distrib(0, 2 * n - 1);
    std::vector<int> rand;
    while(n-->0) {
        rand.push_back(distrib(gen));
    }
    std::sort(rand.begin(), rand.end(), std::greater<int>());
    for(auto i : rand) {
        std::cout << i << std::endl;
    }
    return 0;
}
