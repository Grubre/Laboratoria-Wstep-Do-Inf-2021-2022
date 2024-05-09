#include "dh.hpp"
#include "gf.hpp"
#include <iostream>

auto main() -> int {
    for (auto i = 0; i < 1000; i++) {
        DHSetup<gf> setup;
        User user1(setup);
        User user2(setup);

        auto publicKey1 = user1.getPublicKey();
        auto publicKey2 = user2.getPublicKey();

        user1.setKey(publicKey2);
        user2.setKey(publicKey1);

        auto message = gf{3};
        auto encrypted = user1.encrypt(message);
        auto decrypted = user2.encrypt(encrypted);

        std::cout << message << " " << encrypted << " " << decrypted << std::endl;
    }
    return 0;
}
