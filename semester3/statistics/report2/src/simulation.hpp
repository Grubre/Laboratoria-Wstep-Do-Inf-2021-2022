#pragma once

#include <algorithm>
#include <iostream>
#include <random>
#include <vector>
#include <fstream>

class Simulation
{
public:
    struct Result
    {
        Result() = default;
        Result(int n) {this->n = n;}
        int n;
        int B = -1;
        int U = -1;
        int L = -1;
        int C = -1;
        int D = -1;

        void print_to_stream(std::ofstream& output)
        {
            output << n << ',';
            output << B << ',';
            output << U << ',';
            output << L << ',';
            output << C << ',';
            output << D << ',';
            output << D - C;
            output << std::endl;
        }
    };

    Simulation(unsigned int n)
    {
        this->n = n;
        bins.resize(n,0);
    }

    Result simulate()
    {
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<> distrib(0, n-1);

        Result result(n);

        unsigned int i;
        unsigned int max_val = 0, nr_zeros = n;
        // First we simulate n balls being put in
        // (B)
        for(i = 1; i <= n; i++)
        {
            auto idx = distrib(gen);
            if(bins[idx] == 0)
                nr_zeros--;
            bins[idx]++;
            max_val = std::max(max_val, bins[idx]);
            if(bins[idx] == 2 && result.B == -1)
                result.B = i;
        }
        // (U, L)
        result.U = std::count(bins.begin(), bins.end(), 0);
        // result.L = *std::max_element(bins.begin(), bins.end());
        // result.U = nr_zeros;
        result.L = max_val;


        unsigned int nr_less_than_one = 0, nr_less_than_two = 0;
        for(auto &i : bins)
        {
            if(i == 0)
            {
                nr_less_than_one++;
                nr_less_than_two++;
            }
            else if(i == 1)
                nr_less_than_two++;
        }
        
        // (C, D)
        while(true)
        {
            if(result.C == -1 && nr_less_than_one == 0)
                result.C = i;
            if(result.D == -1 && nr_less_than_two == 0)
            {
                result.D = i;
                break;
            }

            auto idx = distrib(gen);
            if(bins[idx] == 2 && result.B == -1)
                result.B = i;

            if(bins[idx] == 0)
            {
                nr_less_than_one--;
            }
            else if(bins[idx] == 1)
            {
                nr_less_than_two--;
            }
            bins[idx]++;
            
            i++;
        }
        return result;
    }

    void print_bins()
    {
        for(auto& i : bins)
        {
            std::cout << i << '\n';
        }
        std::cout << std::endl;
    }

    void reset()
    {
        bins.clear();
    }

private:
    std::vector<unsigned int> bins;
    unsigned int n;
};
