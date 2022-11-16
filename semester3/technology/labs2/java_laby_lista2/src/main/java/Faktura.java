import java.util.List;

/**
 * Zasady GRASP:
 * Niskie sprzężenie -
 */
public class Faktura {
    private String miejsce_wystawienia;
    private String data_wystawienia;

    private Osoba sprzedawca;
    private Osoba nabywca;

    private List<Towar> towary;

    public Faktura() {}

    public Faktura(String miejsce_wystawienia, String data_wystawienia, Osoba sprzedawca, Osoba nabywca, List<Towar> towary) {
        this.miejsce_wystawienia = miejsce_wystawienia;
        this.data_wystawienia = data_wystawienia;
        this.sprzedawca = sprzedawca;
        this.nabywca = nabywca;
        this.towary = towary;
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

    public List<Towar> getTowary() {
        return towary;
    }

    public void setTowary(List<Towar> towary) {
        this.towary = towary;
    }

    public void addTowar(Towar towar) {
        this.towary.add(towar);
    }
}
