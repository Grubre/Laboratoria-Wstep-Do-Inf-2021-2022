public class Test {
    public static void main(String[] args)
    {
        int n = 1;
        try{
            n = Integer.parseInt(args[0]);
        }catch(Exception e)
        {
            System.out.println("Nieprawidlowy typ ilosci liczb");
            System.exit(0);
        }
        WierszTrojkataPaskala a = new WierszTrojkataPaskala(n);
        for(int i = 1; i < args.length; i++)
        {
            System.out.print(args[i] + " - ");
            try{
                int liczba = Integer.parseInt(args[i]);
                System.out.println(a.wspolczynnik(liczba));
            }
            catch(ArrayIndexOutOfBoundsException e)
            {
                System.out.println("liczba spoza zakresu");
            }
            catch(NumberFormatException e)
            {
                System.out.println("Nieprawidlowa dana");
            }
        }
    }
}
