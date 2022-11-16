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
        for(Towar towar: faktura.getTowary())
        {
            System.out.println(towar.getIlosc() + " x " + towar.getNazwa() + " " + towar.getCena() * towar.getIlosc() + "zl");
        }
        System.out.println("========================================");
    }
}
