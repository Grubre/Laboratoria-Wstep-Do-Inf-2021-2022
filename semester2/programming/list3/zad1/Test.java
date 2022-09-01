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

        Figura[] figury = new Figura[rodzaje.length()];

        int j = 1;
        for(int i = 0; i < rodzaje.length(); i++)
        {
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
                        figury[i] = new Kolo(radius);
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
                            figury[i] = new Kwadrat(bok1, bok2, bok3, bok4, kat);
                        }
                        else if(bok1 == bok3 && bok1 != bok2 && bok2 == bok4 && kat == 90)
                        {
                            figury[i] = new Prostokat(bok1, bok2, bok3, bok4, kat);
                        }
                        else if(bok1 == bok2 && bok2 == bok3 && bok3 == bok4 && kat < 90)
                        {
                            figury[i] = new Romb(bok1, bok2, bok3, bok4, kat);
                        }
                        else if(bok1 == bok2 && bok2 == bok3 && bok3 == bok4 && kat < 180)
                        {
                            figury[i] = new Romb(bok1, bok2, bok3, bok4, 180 - kat);
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
                        figury[i] = new Pieciokat(bok);
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
                        figury[i] = new Szesciokat(bok);
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
        }
        for(int i = 0; i < rodzaje.length(); i++)
        {
            try{
                System.out.println("Rodzaj figury: " + rodzaje.charAt(i));
                System.out.println("Obwod = " + figury[i].obliczObwod());
                System.out.println("Pole = " + figury[i].obliczPole());
            }
            catch(Exception e){
                System.out.println("Blad wczytywania figury" + e);
            };
        }
    }
}
