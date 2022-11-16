import java.io.IOException;

/*
Zasady GRASP sa zachowane w calym kodzie:
1.Expert, controller - klasy ConsoleInterface, ConsoleReader sa wyspecjalizowane
i jako jedyne maja dostep do interakcji z cli
2.Niskie sprzezenie - wiekszosc obiektow o sobie "nie wie", dopiero w miejscach
gdzie jest to konieczne (jak klasa Faktura) przechowywane sa instancje
poszczegolnych obiektow, latwo jednak mozemy zmienic wewnetrzne implementacje
klas bez psucia calosci programu
3.Polimorfizm - klasy Firma i OsobaFizyczna dziedzicza po klasie abstrakcyjnej
Osoba, pozwala to nam na zaoszczedzenie kodu w klasie Faktura i ConsoleReader
4.Pure Fabrication - pozwala na zapisywanie do baz danych przez klasy takie jak
OsobaRepository, etc. ktore dziedzicza po IDatabaseRepository
5.wysoka spojnosc - Bardzo latwo mozemy podmienic interfejs poniewaz klasy takie jak
Faktura, Firma, Osoba, OsobaFizyczna, Towar nie sa powiazane z zadnym frontendem,
kazda z nich posiada tylko ich podstawowa, najwazniejsza funkcjonalnosc
6.Ochrona Zmiennosci - Wszedzie stosowane sa gettery i settery, a zmienne sa prywatne
 */

public class Main {
    public static void main(String[] args) throws IOException {
        ConsoleInterface consoleInterface = new ConsoleInterface();
        consoleInterface.read_faktura();
        while(true) {
            int wybor = consoleInterface.loop_iteration();
            if(wybor == 1) {
                return;
            }
        }
    }
}
