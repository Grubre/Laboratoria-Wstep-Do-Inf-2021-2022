#pragma once
#include <string>
#include <vector>

inline auto longest_common_subsequence(const std::string& str1, const std::string& str2) -> std::string {
    std::vector<std::vector<int>> dp(str1.size() + 1, std::vector<int>(str2.size() + 1, 0));

    for (size_t i = 1; i <= str1.size(); ++i) {
        for (size_t j = 1; j <= str2.size(); ++j) {
            if (str1[i - 1] == str2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = std::max(dp[i - 1][j], dp[i][j - 1]);
            }
        }
    }

    std::string lcs;
    for (size_t i = str1.size(), j = str2.size(); i > 0 && j > 0; ) {
        if (str1[i - 1] == str2[j - 1]) {
            lcs.insert(lcs.begin(), str1[i - 1]);
            --i;
            --j;
        } else if (dp[i - 1][j] > dp[i][j - 1]) {
            --i;
        } else {
            --j;
        }
    }

    return lcs;
}
