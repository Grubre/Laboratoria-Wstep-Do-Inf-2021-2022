public class Test {
    public static void main(String[] args)
    {
        String rodzaje = "";
        try{
            rodzaje = args[0];
        }catch(Exception e)
        {
            System.out.println("Nieprawidlowo podane rodzaje figur");
            System.exit(1);
        }
        int j = 1;
        for(int i = 0; i < rodzaje.length(); i++)
        {
            double pole = 0, obwod = 0;
            switch(rodzaje.charAt(i))
            {
                case 'o':
                {
                    double radius = 0;
                    try{
                        radius = Integer.parseInt(args[j]);
                        if(radius <= 0)
                        {
                            throw new IllegalArgumentException();
                        }
                        obwod = Figury.SingleParameter.Kolo.obliczObwod(radius);
                        pole = Figury.SingleParameter.Kolo.obliczPole(radius);
                    }
                    catch(Exception e){
                        System.out.println("blednie podany promien kola");
                    }
                    j++;
                break;
                }
                case 'c':
                {
                    double bok1 = 0, bok2 = 0, bok3 = 0, bok4 = 0, kat = 0;
                    try{
                        bok1 = Integer.parseInt(args[j]);
                        bok2 = Integer.parseInt(args[j + 1]);
                        bok3 = Integer.parseInt(args[j + 2]);
                        bok4 = Integer.parseInt(args[j + 3]);
                        kat  = Integer.parseInt(args[j + 4]);
                        if(bok1 <= 0 || bok2 <= 0 || bok3 <= 0 || bok4 <= 0 || kat <= 0)
                        {
                            throw new IllegalArgumentException();
                        }
                        if(bok1 == bok2 && bok2 == bok3 && bok3 == bok4 && kat == 90)
                        {
                            obwod = Figury.SingleParameter.Kwadrat.obliczObwod(bok1);
                            pole = Figury.SingleParameter.Kwadrat.obliczPole(bok1);
                        }
                        else if(bok1 == bok3 && bok1 != bok2 && bok2 == bok4 && kat == 90)
                        {
                            obwod = Figury.TwoParameters.Prostokat.obliczObwod(bok1, bok2);
                            pole = Figury.TwoParameters.Prostokat.obliczPole(bok1, bok2);
                        }
                        else if(bok1 == bok2 && bok2 == bok3 && bok3 == bok4 && kat < 90)
                        {
                            obwod = Figury.TwoParameters.Romb.obliczObwod(bok1, kat);
                            pole = Figury.TwoParameters.Romb.obliczPole(bok1, kat);
                        }
                        else if(bok1 == bok2 && bok2 == bok3 && bok3 == bok4 && kat < 180)
                        {
                            obwod = Figury.TwoParameters.Romb.obliczObwod(bok1, 180 - kat);
                            pole = Figury.TwoParameters.Romb.obliczPole(bok1, 180 - kat);
                        }
                        else
                        {
                            throw new IllegalArgumentException();
                        }
                    }
                    catch(Exception e){
                        System.out.println("blednie podane parametry czworokata");
                    }
                    j+=5;
                    break;
                }
                case 'p':
                {
                    double bok = 0;
                    try{
                        bok = Integer.parseInt(args[j]);
                        if(bok <= 0)
                        {
                            throw new IllegalArgumentException();
                        }
                        obwod = Figury.SingleParameter.Pieciokat.obliczObwod(bok);
                        pole = Figury.SingleParameter.Pieciokat.obliczPole(bok);
                    }
                    catch(Exception e){
                        System.out.println("blednie podane parametry pieciokata");
                    }
                    j++;
                    break;
                }
                case 's':
                {
                    double bok = 0;
                    try{
                        bok = Integer.parseInt(args[j]);
                        if(bok <= 0)
                        {
                            throw new IllegalArgumentException();
                        }
                        obwod = Figury.SingleParameter.Szesciokat.obliczObwod(bok);
                        pole = Figury.SingleParameter.Szesciokat.obliczPole(bok);
                    }
                    catch(Exception e){
                        System.out.println("blednie podane parametry Szesciokata");
                    }
                    j++;
                    break;
                }
                default:
                    System.out.println("Nieprawidlowy typ figury");
                break;
            }
            try{
                if(obwod <= 0 || pole <= 0)
                {
                    throw new IllegalArgumentException();
                }
                System.out.println("Rodzaj figury: " + rodzaje.charAt(i));
                System.out.println("Obwod = " + obwod);
                System.out.println("Pole = " + pole);
            }
            catch(Exception e){
                System.out.println("Blad wczytywania figury" + e);
            };
        }
    }
}
