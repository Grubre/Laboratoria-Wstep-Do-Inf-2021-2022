public class Firma extends Osoba {
    private String nip;

    public Firma(String nazwa, String adres, String kod_pocztowy, String nip) {
        super(nazwa, adres, kod_pocztowy);
        this.nip = nip;
    }

    @Override
    public String getFieldsAsString() {
        return nazwa + "\n"+ nip + "\n" + adres + "\n" + kod_pocztowy;
    }
}
