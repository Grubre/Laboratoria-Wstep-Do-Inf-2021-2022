#include <iostream>

int main(int argc, char** argv)
{
    std::string trojkat;

    int n = std::stoi(argv[1]);

    for(int j = 0; j < n; j++)
    {
        std::string row;
        int row_int[n+1];
        row_int[0] = 1;
        for(int i = 1; i <= j; i++)
        {
            row_int[i] = (row_int[i-1] * (j - i + 1)) / i;
        }

        for(int i = 0; i <= j; i++)
        {
            row+=std::to_string(row_int[i]);
            if(i != j)
                row+=' ';
        }
        trojkat = trojkat + row + ((j != n - 1) ? "\n" : "");
    }
    std::cout << trojkat << std::endl;
    return 0;
}