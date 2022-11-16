public class OsobaFizyczna extends Osoba {
    private String pesel;

    public OsobaFizyczna(String nazwa, String adres, String kod_pocztowy, String pesel) {
        super(nazwa, adres, kod_pocztowy);
        this.pesel = pesel;
    }

    @Override
    public String getFieldsAsString() {
        return nazwa + "\n"+ pesel + "\n" + adres + "\n" + kod_pocztowy;
    }
}
