public class Towar {
    private Integer ilosc;
    private String nazwa;
    private Integer cena;

    public Towar(Integer ilosc, String nazwa, Integer cena) {
        this.ilosc = ilosc;
        this.nazwa = nazwa;
        this.cena = cena;
    }

    public Integer getIlosc() {
        return ilosc;
    }

    public void setIlosc(Integer ilosc) {
        this.ilosc = ilosc;
    }

    public String getNazwa() {
        return nazwa;
    }

    public void setNazwa(String nazwa) {
        this.nazwa = nazwa;
    }

    public Integer getCena() {
        return cena;
    }

    public void setCena(Integer cena) {
        this.cena = cena;
    }
}
