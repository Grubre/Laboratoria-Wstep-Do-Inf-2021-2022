import java.util.Random;
import java.util.ArrayList;

public class DHSetup {
    private gf generator;

    public DHSetup(int p) {
        generator = new gf(p, 2);
    }

    public gf getGenerator() {
        return generator;
    }

    public gf power(gf a, int b) {
        gf result = new gf(generator.characteristic(), 1);
        while(b > 0) {
            if(b % 2 == 1) {
                result = result.mul(a);
            }
            a = a.mul(a);
            b /= 2;
        }
        return result;
    }

    private gf get_random_generator(int p) {
        gf generator = new gf(p, 1);
        Random rand = new Random();

        ArrayList<Integer> divisors = get_prime_divisors(p - 1);

        Boolean is_generator = false;
        while(!is_generator) {
            is_generator = true;
            int candidate = rand.nextInt(p - 2) + 1;
            generator = new gf(p, candidate);
            for(Integer q : divisors) {
                gf mult = new gf(p, (p - 1) / q);
                if(generator.mul(mult).equals(new gf(p, 1))) {
                    is_generator = false;
                    break;
                }
            }
        }

        return generator;
    }

    private ArrayList<Integer> get_prime_divisors(int n) {
        int i = 2;
        int j = 0;
        ArrayList<Integer> divisors = new ArrayList<Integer>();

        while(i * i <= n) {
            if(n % i == 0 && is_prime(i)) {
                divisors.add(i);
                while (n % i == 0) {
                    n /= i;
                }
            }
        }

        if (n != 1) {
            divisors.add(i);
        }

        return divisors;
    }

    private Boolean is_prime(int n) {
        if(n <= 1) {
            return false;
        }
        if(n <= 3) {
            return true;
        }
        if(n % 2 == 0 || n % 3 == 0) {
            return false;
        }

        int i = 5;
        while(i * i <= n) {
            if(n % i == 0 || n % (i + 2) == 0) {
                return false;
            }
            i += 6;
        }
        return true;
    }
}
