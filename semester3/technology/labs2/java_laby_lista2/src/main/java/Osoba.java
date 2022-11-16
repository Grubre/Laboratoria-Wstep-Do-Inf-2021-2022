public abstract class Osoba {
    protected String nazwa;
    protected String adres;
    protected String kod_pocztowy;

    protected Osoba(String nazwa, String adres, String kod_pocztowy) {
        this.nazwa = nazwa;
        this.adres = adres;
        this.kod_pocztowy = kod_pocztowy;
    }

    public abstract String getFieldsAsString();
}
