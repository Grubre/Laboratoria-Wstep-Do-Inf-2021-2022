import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Vector;

public class ConsoleReader {
    private BufferedReader reader;

    public ConsoleReader() {
         this.reader = new BufferedReader(
                new InputStreamReader(System.in));
    }
    public String read_miejsce() throws IOException {
        System.out.println("Podaj miejsce wystawienia: ");

        String input = reader.readLine();

        return input;
    }

    public String read_data() throws IOException {
        System.out.println("Podaj date wystawienia: ");

        String input = reader.readLine();

        return input;
    }

    public OsobaFizyczna read_osoba_fizyczna() throws IOException {
        System.out.println("Podaj dane osoby fizycznej: ");

        String nazwa;
        String adres;
        String kod_pocztowy;
        String pesel;

        System.out.print("Podaj nazwe: ");
        nazwa = reader.readLine();
        System.out.print("Podaj adres: ");
        adres = reader.readLine();
        System.out.print("Podaj kod pocztowy: ");
        kod_pocztowy = reader.readLine();
        System.out.print("Podaj pesel: ");
        pesel = reader.readLine();

        return new OsobaFizyczna(nazwa,adres,kod_pocztowy,pesel);
    }

    public Firma read_firma() throws IOException {
        System.out.println("Podaj dane firmy: ");

        String nazwa;
        String adres;
        String kod_pocztowy;
        String nip;

        System.out.print("Podaj nazwe: ");
        nazwa = reader.readLine();
        System.out.print("Podaj adres: ");
        adres = reader.readLine();
        System.out.print("Podaj kod pocztowy: ");
        kod_pocztowy = reader.readLine();
        System.out.print("Podaj NIP: ");
        nip = reader.readLine();

        return new Firma(nazwa,adres,kod_pocztowy,nip);
    }

    public Towar read_towar() throws IOException {
        System.out.println("Podaj dane towaru: ");

        String ilosc;
        String nazwa;
        String cena;

        try {
            System.out.print("Podaj ilosc: ");
            ilosc = reader.readLine();
            System.out.print("Podaj nazwe: ");
            nazwa = reader.readLine();
            System.out.print("Podaj cene za sztuke: ");
            cena = reader.readLine();

            return new Towar(Integer.parseInt(ilosc),nazwa,Integer.parseInt(cena));
        } catch(Exception e) {
            System.out.println("[ERROR]: Ilosc oraz cena musza byc liczbami!");
        }

        return null;
    }

    public Faktura read_faktura() throws IOException {
        String miejsce_wystawienia;
        String data_wystawienia;

        Osoba sprzedawca;
        Osoba nabywca;

        String wybor;

        miejsce_wystawienia = read_miejsce();
        data_wystawienia = read_data();

        try {
            System.out.println("Sprzedawca jest osoba fizyczna? (y/n)");
            wybor = reader.readLine();
            if("y".equals(wybor)){
                sprzedawca = read_osoba_fizyczna();
            }
            else if("n".equals(wybor)) {
                sprzedawca = read_firma();
            }
            else {
                throw new IllegalArgumentException();
            }
            System.out.println("Nabywca jest osoba fizyczna? (y/n)");
            wybor = reader.readLine();
            if("y".equals(wybor)){
                nabywca = read_osoba_fizyczna();
            }
            else if("n".equals(wybor)){
                nabywca = read_firma();
            }
            else {
                throw new IllegalArgumentException();
            }
            return new Faktura(miejsce_wystawienia, data_wystawienia, sprzedawca, nabywca, new Vector<Towar>());
        } catch(IllegalArgumentException e) {
            System.out.println("[ERROR]: Musisz wybrac miedzy y/n!");
        }

        return null;
    }
}
