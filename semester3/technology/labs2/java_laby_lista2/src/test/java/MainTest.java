import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

//todo

class MainTest {
    @Test
    void demoTestMethod() {
        Towar towar = new Towar("abc", 2);
        Integer ilosc = 2;
        Faktura.Wiersz wiersz = new Faktura.Wiersz(towar, ilosc);
        assertTrue(ilosc * towar.getCena() == wiersz.getFullCena());
    }
}