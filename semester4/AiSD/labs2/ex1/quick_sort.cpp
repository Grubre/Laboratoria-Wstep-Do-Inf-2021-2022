#include <iostream>
#include <vector>
#include <stack>


auto print_vector(const std::vector<int>& numbers) -> void {
    std::cout << "(";
    for(int i = 0; i < numbers.size(); i++) {
        std::cout << numbers[i];
        if(i < numbers.size() - 1) {
            std::cout << ", ";
        }
    }
    std::cout << ")";
}


auto partition(std::vector<int>& numbers, int l, int h) -> int {
    auto x = numbers[h];
    auto i = l - 1;

    for(int j = l; j <= h - 1; j++) {
        if(numbers[j] <= x) {
            i++;
            std::swap(numbers[i], numbers[j]);
        }
    }

    std::swap(numbers[i + 1], numbers[h]);
    return i + 1;
}


auto quick_sort(const std::vector<int>& numbers) -> std::vector<int> {
    std::vector<int> ret = numbers;

    std::stack<int> stack;

    stack.push(0);
    stack.push(ret.size() - 1);

    while(!stack.empty()) {
        print_vector(ret);
        std::cout << std::endl;
        auto h = stack.top();
        stack.pop();
        auto l = stack.top();
        stack.pop();

        auto p = partition(ret, l, h);

        if(p - 1 > l) {
            stack.push(l);
            stack.push(p - 1);
        }

        if(p + 1 < h) {
            stack.push(p + 1);
            stack.push(h);
        }
    }

    return ret;
}

auto main(int argc, char** argv) -> int {
    std::vector<int> numbers;
    numbers.reserve(argc);
    for(int i = 1; i < argc; i++) {
        numbers.push_back(std::stoi(argv[i]));
    }

    std::cout << "dane wejsciowe: ";
    print_vector(numbers);
    std::cout << std::endl;

    std::vector<int> sorted = quick_sort(numbers);

    std::cout << "posortowana tablica: ";
    print_vector(sorted);
    std::cout << std::endl;
    return 0;
}
