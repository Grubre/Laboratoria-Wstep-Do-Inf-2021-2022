#pragma once
#include <vector>
#include <algorithm>


auto select_index(std::vector<int>& nums, int low, int high, int index) -> int {
    int idx = 0;
    std::vector<int> copy;
    for(int i = low; i <= high; i++) {
        copy.push_back(nums[i]);
    }
    std::sort(copy.begin(), copy.end());
    return idx;
}

