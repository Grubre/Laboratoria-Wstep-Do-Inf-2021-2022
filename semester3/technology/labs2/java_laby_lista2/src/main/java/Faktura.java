import java.util.List;
import java.util.Vector;

/**
 * Zasady GRASP:
 * Niskie sprzężenie -
 */
public class Faktura {
    public static class Wiersz {
        public Towar towar;
        public Integer ilosc;
        Wiersz(Towar towar, Integer ilosc) {
            this.towar = towar;
            this.ilosc = ilosc;
        }
        Integer getFullCena() {
            return towar.getCena() * ilosc;
        }
    }
    private String miejsce_wystawienia;
    private String data_wystawienia;

    private Osoba sprzedawca;
    private Osoba nabywca;

    private List<Wiersz> towary;

    public Faktura() {}

    public Faktura(String miejsce_wystawienia, String data_wystawienia, Osoba sprzedawca, Osoba nabywca) {
        this.miejsce_wystawienia = miejsce_wystawienia;
        this.data_wystawienia = data_wystawienia;
        this.sprzedawca = sprzedawca;
        this.nabywca = nabywca;
        towary = new Vector<>();
    }

    public String getMiejsce_wystawienia() {
        return miejsce_wystawienia;
    }

    public void setMiejsce_wystawienia(String miejsce_wystawienia) {
        this.miejsce_wystawienia = miejsce_wystawienia;
    }

    public String getData_wystawienia() {
        return data_wystawienia;
    }

    public void setData_wystawienia(String data_wystawienia) {
        this.data_wystawienia = data_wystawienia;
    }

    public Osoba getSprzedawca() {
        return sprzedawca;
    }

    public void setSprzedawca(Osoba sprzedawca) {
        this.sprzedawca = sprzedawca;
    }

    public Osoba getNabywca() {
        return nabywca;
    }

    public void setNabywca(Osoba nabywca) {
        this.nabywca = nabywca;
    }

    public List<Wiersz> getTowary() {
        return towary;
    }

    public void setTowary(List<Wiersz> towary) {
        this.towary = towary;
    }

    public void addTowar(Wiersz towar) {
        this.towary.add(towar);
    }
}
