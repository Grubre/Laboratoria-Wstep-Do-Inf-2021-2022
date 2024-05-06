import gf;
import dh;
import std.random;

struct User(alias T) {
    this(DHSetup!T* setup) {
        this.setup = setup;
        secret = uniform(1, T.characteristic() - 1);
    }

    T getPublicKey() {
        return setup.power(setup.getGenerator(), secret);
    }

    void setKey(T a) {
        key = setup.power(a, secret);
    }

    T encrypt(T m) {
        return m * key;
    }

    T decrypt(T c) {
        return c / key;
    }

    DHSetup!T* setup;
    uint secret;
    T key;
}
