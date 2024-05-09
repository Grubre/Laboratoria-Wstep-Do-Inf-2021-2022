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

        this(T val) {
            if (val < 0) {
                value = p - (- val) % p;
            } else {
                value = val % p;
            }
        }

        public static T characteristic() { return p; }

        GF inv() {
            if (value == 0) throw new Exception("Inverse of zero");

            T t = 0, r = p, tt = 1, rt = value;

            while (rt != 0) {
                T q = r / rt;

                T temp = t - q * tt;
                t = tt;
                tt = temp;

                temp = r - q * rt;
                r = rt;
                rt = temp;
            }

            if (t < 0) t += p;

            return GF(t);
        }

        GF opBinary(string op)(GF rhs) {
            static if (op == "/") {
                if (rhs.value == 0) throw new Exception("Division by zero");
                return this * rhs.inv();
            }
            static if (op == "-") {
                return GF(p + value - rhs.value);
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
