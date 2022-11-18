import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Objects;

public class ConsoleInterface {
    private ConsoleReader consoleReader;
    private Faktura faktura;
    private BufferedReader reader;
    private Printer printer;

    public ConsoleInterface() {
        this.consoleReader = new ConsoleReader();
        this.printer = new Printer();
        this.reader = new BufferedReader(
                new InputStreamReader(System.in));
    }

    public void read_faktura() {
        while(true)
        {
            try {
                faktura = consoleReader.read_faktura();
                if(faktura != null)
                    break;
            } catch(Exception e) {
                System.out.println("Blad przy wczytywaniu faktury!");
            }
        }
    }

    public int loop_iteration() {
        System.out.println("Wpisz: ");
        System.out.println("1. Dodaj towar");
        System.out.println("2. Wypisz fakture");
        System.out.println("3. Wyjdz z programu");
        String wybor;
        try {
            wybor = reader.readLine();
            if(!(Objects.equals(wybor, "1") || Objects.equals(wybor, "2") || Objects.equals(wybor, "3"))){
                throw new IllegalArgumentException();
            }
            switch (wybor) {
                case "1":
                    System.out.println("Podaj dane towaru: ");
                    System.out.print("Podaj ilosc: ");
                    String val = reader.readLine();
                    Integer ilosc = Integer.parseInt(val);
                    Towar towar = consoleReader.read_towar();
                    if(towar != null)
                        faktura.addTowar(new Faktura.Wiersz(towar, ilosc));
                    break;
                case "2":
                    printer.print_faktura_to_console(faktura);
                    break;
                case "3":
                    return 1;
            }
        } catch( IllegalArgumentException e) {
            System.out.println("[ERROR]: Musisz wybrac 1, 2 lub 3!");
        } catch (IOException e) {
            System.out.println("[ERROR]: Zly input!");
        }
        return 0;

    }

}
