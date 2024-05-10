import java.util.Random;

public class User {
    public User(DHSetup dhsetup) {
        this.dhsetup = dhsetup;

        Random rand = new Random();
        secret = rand.nextInt(dhsetup.getGenerator().characteristic() - 2) + 1;
    }

    public gf getPublicKey() {
        return dhsetup.power(dhsetup.getGenerator(), secret);
    }

    public void setKey(gf a) {
        this.key = dhsetup.power(a, secret);
        key_set = true;
    }

    public gf encrypt(gf m) {
        if(!key_set) {
            throw new IllegalStateException("Key not set");
        }
        return m.mul(key);
    }

    public gf decrypt(gf c) {
        if(!key_set) {
            throw new IllegalStateException("Key not set");
        }
        return c.div(key);
    }

    private DHSetup dhsetup;
    int secret;
    gf key;
    boolean key_set = false;
}
