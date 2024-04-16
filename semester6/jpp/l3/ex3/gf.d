import std.stdio;

bool is_prime(int n) {
    if (n < 2) return false;
    if (n < 4) return true;
    if (n % 2 == 0) return false;
    for (int i = 3; i * i <= n; i += 2) {
        if (n % i == 0) return false;
    }
    return true;
}

template GF(alias p, T = uint) if (is_prime(p)) {
    struct GF {
        T value = 0;

        this(T val) { value = val % p; }

        T characteristic() { return p; }

        GF inv() {
            T a = value, b = p, x = 0, y = 1, t;
            while (a != 0) {
                t = a;
                a = b % a;
                b = t;
                t = x;
                x = y - b / t * x;
                y = t;
            }
            return GF(y);
        }

        GF opBinary(string op)(GF rhs) {
            static if (op == "/") {
                if (rhs.value == 0) throw new Exception("Division by zero");
                return this * rhs.inv();
            }
            mixin("return GF(cast(T)(value " ~ op ~ " rhs.value));");
        }

        GF opUnary(string op)() if (op == "-") {
            return GF(p - value);
        }

        bool opEquals(GF rhs) {
            return value == rhs.value;
        }

        string toString() {
            import std.conv;
            return value.to!string;
        }
    }
}
