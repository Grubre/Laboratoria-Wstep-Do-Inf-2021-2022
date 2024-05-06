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

    public int characteristic() {
        return p;
    }

    public gf neg() {
        return new gf(p, p - a);
    }

    public gf inv() {
        if (a == 0) {
            throw new IllegalArgumentException("Inverse of zero does not exist");
        }

        int t = 0, r = p, tt = 1, rt = a;

        while (rt != 0) {
            int q = r / rt;

            int temp = t - q * tt;
            t = tt;
            tt = temp;

            temp = r - q * rt;
            r = rt;
            rt = temp;
        }

        if (t < 0) {
            t += p;
        }

        return new gf(p, t);
    }

    public gf add(gf b) {
        if (p != b.characteristic()) {
            throw new IllegalArgumentException("Fields are not compatible");
        }

        return new gf(p, (a + b.get()) % p);
    }

    public gf sub(gf b) {
        if (p != b.characteristic()) {
            throw new IllegalArgumentException("Fields are not compatible");
        }

        return new gf(p, (a - b.get() + p) % p);
    }

    public gf mul(gf b) {
        if (p != b.characteristic()) {
            throw new IllegalArgumentException("Fields are not compatible");
        }

        return new gf(p, (a * b.get()) % p);
    }

    public gf div(gf b) {
        if (p != b.characteristic()) {
            throw new IllegalArgumentException("Fields are not compatible");
        }

        if (b.get() == 0) {
            throw new IllegalArgumentException("Inverse of zero does not exist");
        }
        
        return new gf(p, (a * b.inv().get()) % p);
    }

    public Boolean equals(gf b) {
        if (p != b.characteristic()) {
            throw new IllegalArgumentException("Fields are not compatible");
        }

        return a == b.get();
    }

    public String toString() {
        return Integer.toString(a);
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
