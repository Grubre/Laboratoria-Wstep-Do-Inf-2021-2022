import java.util.Random;
import java.util.ArrayList;

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
    }

    public gf encrypt(gf m) {
        return m.mul(key);
    }

    public gf decrypt(gf c) {
        return c.div(key);
    }

    private DHSetup dhsetup;
    int secret;
    gf key;
}
