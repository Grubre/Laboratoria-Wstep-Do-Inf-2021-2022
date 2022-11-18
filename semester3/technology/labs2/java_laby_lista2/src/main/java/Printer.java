public class Printer {
    public Printer() {

    }
    public void print_faktura_to_console(Faktura faktura) {
        System.out.println("========================================");
        System.out.println("Data: "+faktura.getData_wystawienia());
        System.out.println("Miejsce: "+faktura.getMiejsce_wystawienia());
        System.out.println("========================================");
        System.out.println("Sprzedawca:");
        System.out.println(faktura.getSprzedawca().getFieldsAsString());
        System.out.println("========================================");
        System.out.println("Nabywca:");
        System.out.println(faktura.getNabywca().getFieldsAsString());
        System.out.println("========================================");
        Integer calaCena = 0;
        for(Faktura.Wiersz towar: faktura.getTowary())
        {
            System.out.println(towar.ilosc + " x " + towar.towar.getNazwa() + " " + towar.getFullCena() + " zl");
            calaCena += towar.getFullCena();
        }
        System.out.println("Cena calkowita: " + calaCena + " zl");
        System.out.println("========================================");
    }
}
