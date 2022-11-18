public class Towar {
    private String nazwa;
    private Integer cena;

    public Towar(String nazwa, Integer cena) {
        this.nazwa = nazwa;
        this.cena = cena;
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
