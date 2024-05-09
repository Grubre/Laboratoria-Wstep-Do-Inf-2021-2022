import gf;
import std.random;

struct DHSetup(alias T) {
    private T generator;
    private static uint[] divisors;

    static DHSetup OpCall() {
        DHSetup!T a;
        a.generator = get_random_generator();
        return a;
    }

    T getGenerator() {
        return generator;
    }

    T power(T a, uint b) {
        T result = T(1);
        while (b > 0) {
            if (b % 2 == 1) {
                result = result * a;
            }
            a = a * a;
            b /= 2;
        }
        return result;
    }

    static this() {
        divisors = get_prime_divisors(T.characteristic() - 1);
    }

    static private T get_random_generator() {
        uint p = T.characteristic();

        T gen;

        bool is_generator = false;
        while (!is_generator) {
            is_generator = true;
            gen = T(uniform(1, p - 1)); 
            foreach (q; divisors) {
                if (gen * T(((p - 1) / q)) == T(1)) {
                    is_generator = false;
                    break;
                }
            }

        }

        return gen;
    }

    static private uint[] get_prime_divisors(uint n) {
        uint p = T.characteristic();
        uint i = 2;
        uint[] divisors;

        while(i * i <= p) {
            if (p % i == 0 && is_prime(i)) {
                divisors ~= i;
                while (p % i == 0) {
                    p /= i;
                }
            }
            i++;
        }

        if (p != 1) {
            divisors ~= p;
        }

        return divisors;
    }
}
