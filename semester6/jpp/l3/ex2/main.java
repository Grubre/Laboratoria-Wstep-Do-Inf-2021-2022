public class main {
    public static void main(String[] args) {
        gf a = new gf(1234577, 5);
        gf b = new gf(1234577, 7);

        gf sum = a.add(b);
        gf diff = a.sub(b);
        gf prod = a.mul(b);
        gf quot = a.div(b);

        System.out.println("a = " + a);
        System.out.println("b = " + b);
        System.out.println("(1 / b) * b = " + b.inv().mul(b));
        System.out.println("a + b = " + sum);
        System.out.println("a - b = " + diff);
        System.out.println("a * b = " + prod);
        System.out.println("(a / b) * b = " + quot.mul(b));
    }
}