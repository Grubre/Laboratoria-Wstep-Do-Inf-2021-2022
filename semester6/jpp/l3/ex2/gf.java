public class gf {
    private int a = 0;
    private int p;

    public gf(int p, int a) {
        if(!is_prime(p)) {
            throw new IllegalArgumentException("p is not prime");
        }
        this.p = p;
        this.a = a % p;
    }

    public int get() {
        return a % p;
    }

    public void set(int a) {
        this.a = a % p;
    }

    public gf neg() {
        return new gf(p, p - a);
    }

    public gf inv() {
        int t = 0;
        int newt = 1;
        int r = p;
        int newr = a;
        int q, tmp;
        while (newr != 0) {
            q = r / newr;
            tmp = newt;
            newt = t - q * newt;
            t = tmp;
            tmp = newr;
            newr = r - q * newr;
            r = tmp;
        }
        if (r > 1) {
            throw new IllegalArgumentException("a is not invertible");
        }
        if (t < 0) {
            t += p;
        }
        return new gf(p, t);
    }

    public gf add(gf b) {
        return new gf(p, (a + b.get()) % p);
    }

    public gf sub(gf b) {
        return new gf(p, (a - b.get() + p) % p);
    }

    public gf mul(gf b) {
        return new gf(p, (a * b.get()) % p);
    }

    public gf div(gf b) {
        return new gf(p, (a * b.inv().get()) % p);
    }

    private Boolean is_prime(int p) {
        for (int i = 2; i < p; i++) {
            if (p % i == 0) {
                return false;
            }
        }
        return true;
    }
}
