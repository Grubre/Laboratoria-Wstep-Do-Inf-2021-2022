#include <iostream>

int main(int argc, char** argv)
{
    std::string row;

    int n = std::stoi(argv[1]);
    int row_int[n+1];
    row_int[0] = 1;
    for(int i = 1; i <= n; i++)
    {
        row_int[i] = (row_int[i-1] * (n - i + 1)) / i;
    }

    for(int i = 0; i <= n; i++)
    {
        row+=std::to_string(row_int[i]);
        if(i != n)
            row+=' ';
    }
    //row+='\n';

    std::cout << row << std::endl;
    return 0;
}