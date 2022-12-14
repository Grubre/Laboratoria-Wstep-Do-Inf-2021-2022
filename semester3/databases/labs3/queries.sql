1:
-- Stworzenie bazy oraz tabel
    CREATE DATABASE firma;
    CREATE TABLE Ludzie(
        czlowiek_id int unsigned NOT NULL AUTO_INCREMENT PRIMARY KEY,
        PESEL char(11),
        imie varchar(30) NOT NULL,
        nazwisko varchar(30) NOT NULL,
        data_urodzenia date NOT NULL,
        plec enum('K', 'M') NOT NULL
    );
    CREATE TABLE Zawody(
        zawod_id int unsigned NOT NULL AUTO_INCREMENT PRIMARY KEY,
        nazwa varchar(50) NOT NULL,
        pensja_min float NOT NULL CHECK (pensja_min >= 0),
        pensja_max float NOT NULL CHECK (pensja_min < pensja_max)
    );
    CREATE TABLE Pracownicy(
        czlowiek_id int unsigned NOT NULL,
        zawod_id int unsigned NOT NULL,
        pensja float NOT NULL CHECK (pensja >= 0),
        PRIMARY KEY (czlowiek_id, zawod_id),
        FOREIGN KEY (czlowiek_id) REFERENCES Ludzie(czlowiek_id),
        FOREIGN KEY (zawod_id) REFERENCES Zawody(zawod_id)
    );
-- Tutaj tworzymy funkcje wykorzystywana do sprawdzania poprawnosci peselu
-- pozwala ona uniknac duplikacji kodu przy sprawdzaniu insertu i update'u
    DELIMITER $$
    CREATE FUNCTION check_pesel(pesel char(11), plec enum('K','M'), data_urodzenia date)
    RETURNS bool DETERMINISTIC
    BEGIN
        -- Pesel jest w formacie RRMMDDPPPPK
        -- Deklaracje
        DECLARE rok char(4);
        DECLARE miesiac int;
        DECLARE dzien int;
        DECLARE rok_offset int;
        DECLARE k int;
        DECLARE p int;
        SET rok = YEAR(data_urodzenia);
        SET miesiac = MONTH(data_urodzenia);
        SET dzien = DAYOFMONTH(data_urodzenia);

        -- Najpierw sprawdzamy rok (RR)
        IF (SUBSTR(rok, 3, 2) <> SUBSTR(pesel, 1, 2)) THEN
            RETURN false;
        END IF;

        -- Potem miesiac zgodnie z tabela (MM)
        IF (rok BETWEEN "1800" AND "1899") THEN
            SET rok_offset = 80;
        END IF;
        IF (rok BETWEEN "1900" AND "1999") THEN
            SET rok_offset = 0;
        END IF;
        IF (rok BETWEEN "2000" AND "2099") THEN
            SET rok_offset = 20;
        END IF;
        IF (miesiac + rok_offset <> SUBSTR(pesel, 3, 2)) THEN
            RETURN false;
        END IF;

        -- Dzien (DD)
        IF (dzien <> SUBSTR(pesel, 5, 2)) THEN
            RETURN false;
        END IF;

        -- Potem czy ostatnia cyfra P zgadza sie z plcia (PPPP)
        SET p = SUBSTR(pesel, 10, 1);
        IF (plec = 'K' AND (p % 2) <> 0) THEN
            return false;
        END IF;
        IF (plec = 'M' AND (p % 2) <> 1) THEN
            return false;
        END IF;

        -- I na koncu K (K)
        SET k = (SUBSTR(pesel,1,1) * 1) % 10 +
                (SUBSTR(pesel,2,1) * 3) % 10 +
                (SUBSTR(pesel,3,1) * 7) % 10 +
                (SUBSTR(pesel,4,1) * 9) % 10 +
                (SUBSTR(pesel,5,1) * 1) % 10 +
                (SUBSTR(pesel,6,1) * 3) % 10 +
                (SUBSTR(pesel,7,1) * 7) % 10 +
                (SUBSTR(pesel,8,1) * 9) % 10 +
                (SUBSTR(pesel,9,1) * 1) % 10 +
                (SUBSTR(pesel,10,1) * 3)% 10;
        SET k = (10 - (k % 10)) % 10;
        IF (k <> SUBSTR(pesel FROM 11 FOR 1)) THEN
            RETURN false;
        END IF;

        RETURN true;
    END$$
    DELIMITER ;

    -- Funkcja tworzaca PESEL dla danej osoby uzywana w triggerze
    DROP FUNCTION make_pesel_for;
    DELIMITER $$
    CREATE FUNCTION make_pesel_for(data_urodzenia date, plec enum('K','M'))
    RETURNS char(11)
    BEGIN
        DECLARE rok char(4);
        DECLARE miesiac int;
        DECLARE dzien int;
        DECLARE rok_offset int;
        DECLARE k int;
        DECLARE p int;
        DECLARE pesel char(11);
        SET rok = YEAR(data_urodzenia);
        SET miesiac = MONTH(data_urodzenia);
        SET dzien = DAYOFMONTH(data_urodzenia);

        SET pesel = SUBSTR(rok, 3, 2);
        
        IF (rok BETWEEN "1800" AND "1899") THEN
            SET rok_offset = 80;
        END IF;
        IF (rok BETWEEN "1900" AND "1999") THEN
            SET rok_offset = 0;
        END IF;
        IF (rok BETWEEN "2000" AND "2099") THEN
            SET rok_offset = 20;
        END IF;

        IF (miesiac + rok_offset < 10) THEN
            SET pesel = CONCAT(pesel, '0');
        END IF;
        SET pesel = CONCAT(pesel, miesiac + rok_offset);
        
        IF (dzien < 10) THEN
            SET pesel = CONCAT(pesel, '0');
        END IF;
        SET pesel = CONCAT(pesel, dzien);

        SET pesel = CONCAT(pesel, "000");

        IF (plec = 'K') THEN
            SET pesel = CONCAT(pesel, "0");
        END IF;
        IF (plec = 'M') THEN
            SET pesel = CONCAT(pesel, "1");
        END IF;

        SET k = (SUBSTR(pesel,1,1) * 1) % 10 +
                (SUBSTR(pesel,2,1) * 3) % 10 +
                (SUBSTR(pesel,3,1) * 7) % 10 +
                (SUBSTR(pesel,4,1) * 9) % 10 +
                (SUBSTR(pesel,5,1) * 1) % 10 +
                (SUBSTR(pesel,6,1) * 3) % 10 +
                (SUBSTR(pesel,7,1) * 7) % 10 +
                (SUBSTR(pesel,8,1) * 9) % 10 +
                (SUBSTR(pesel,9,1) * 1) % 10 +
                (SUBSTR(pesel,10,1) * 3)% 10;
        SET k = (10 - (k % 10)) % 10;
        SET pesel = CONCAT(pesel, k);
        RETURN pesel;
    END$$

    -- Tutaj tworze triggery
    DELIMITER $$
    CREATE TRIGGER pesel_err_on_insert
    BEFORE INSERT
    ON Ludzie FOR EACH ROW
    BEGIN
        declare msg varchar(128);
        IF check_pesel(new.PESEL, new.plec, new.data_urodzenia) THEN
            SET new.PESEL = make_pesel_for(new.data_urodzenia, new.plec);
        ELSE
            set msg = concat('PeselCheckError: Trying to insert ', new.IMIE, ' ', new.NAZWISKO ,' with invalid PESEL: ', new.PESEL);
            signal sqlstate '45000' set message_text = msg;
        END IF;
    END$$
    DELIMITER ;
    DELIMITER $$
    CREATE TRIGGER pesel_err_on_update AFTER UPDATE ON Ludzie
    FOR EACH ROW
    BEGIN
        declare msg varchar(128);
        IF NOT check_pesel(new.PESEL, new.plec, new.data_urodzenia) THEN
            set msg = concat('PeselCheckError: Trying to update ', new.IMIE, ' ', new.NAZWISKO ,' with invalid PESEL: ', new.PESEL);
            signal sqlstate '45000' set message_text = msg;
        END IF;
    END$$
    DELIMITER ;
    

    -- Czesc druga

    -- Wypelnienie tabel
    -- [1,17], 5 entries
    INSERT INTO Ludzie(imie, nazwisko, data_urodzenia, plec) VALUES
    ("Kewin","Cieślak","2006-11-21","M"),
    ("Eugeniusz","Wójcik","2011-08-25","M"),
    ("Marcin","Włodarczyk","2013-10-01","M"),
    ("Florencja","Zalewska","2019-04-07","K"),
    ("Marlena","Błaszczyk","2019-06-13","K");

    -- [18,59], 45 entries
    INSERT INTO Ludzie(imie, nazwisko, data_urodzenia, plec) VALUES
    ("Michał","Sobczak","1965-03-07","M"),
    ("Maurycy","Walczak","1967-04-20","M"),
    ("Alexander","Kwiatkowski","1968-06-28","M"),
    ("Maurycy","Witkowski","1970-08-14","M"),
    ("Albert","Zieliński","1972-10-20","M"),
    ("Hubert","Adamska","1975-07-23","M"),
    ("Dariusz","Zakrzewska","1976-10-22","M"),
    ("Andrzej","Wiśniewski","1978-03-23","M"),
    ("Marcel","Sikorska","1978-11-14","M"),
    ("Jarosław","Witkowski","1981-10-22","M"),
    ("Patryk","Kowalczyk","1983-02-24","M"),
    ("Allan","Kowalczyk","1983-10-18","M"),
    ("Łukasz","Lewandowski","1993-03-06","M"),
    ("Fryderyk","Mazurek","1993-03-17","M"),
    ("Fabian","Głowacka","1997-05-05","M"),
    ("Mariusz","Laskowska","1999-11-14","M"),
    ("Robert","Stępień","1999-12-07","M"),
    ("Albert","Szewczyk","2000-02-04","M"),
    ("Olgierd","Wysocki","2000-02-24","M"),
    ("Mieszko","Nowak","2002-06-07","M"),
    ("Józef","Piotrowski","1965-01-25","M"),
    ("Celina","Duda","1966-05-03","K"),
    ("Irena","Marciniak","1972-01-30","K"),
    ("Nikola","Zalewska","1972-05-20","K"),
    ("Lidia","Kwiatkowska","1973-11-13","K"),
    ("Sylwia","Szymańska","1975-11-30","K"),
    ("Oliwia","Zakrzewska","1977-01-29","K"),
    ("Marcelina","Tomaszewska","1977-03-27","K"),
    ("Liliana","Szczepańska","1978-03-28","K"),
    ("Alicja","Lewandowska","1978-07-30","K"),
    ("Luiza","Laskowska","1979-02-07","K"),
    ("Patrycja","Kaczmarczyk","1980-03-12","K"),
    ("Wiktoria","Kozłowska","1981-05-22","K"),
    ("Eliza","Laskowska","1982-01-23","K"),
    ("Anatolia","Szulc","1982-05-24","K"),
    ("Lila","Chmielewska","1985-11-21","K"),
    ("Danuta","Głowacka","1986-06-03","K"),
    ("Danuta","Pietrzak","1989-04-17","K"),
    ("Wiktoria","Czarnecka","1990-09-16","K"),
    ("Lara","Laskowska","1992-10-23","K"),
    ("Asia","Piotrowska","1993-02-13","K"),
    ("Jowita","Bąk","1993-03-07","K"),
    ("Anita","Maciejewska","1994-12-18","K"),
    ("Jadwiga","Górska","2001-01-02","K"),
    ("Maria","Duda","2001-02-25","K");

    -- [60,inf+), 5 entries
    INSERT INTO Ludzie(imie, nazwisko, data_urodzenia, plec) VALUES
    ("Ludwik","Kowalski","1949-04-17","M"),
    ("Marysia","Szymczak","1935-11-30","K"),
    ("Oliwia","Szymańska","1956-01-02","K"),
    ("Otylia","Borkowska","1960-06-07","K"),
    ("Ewelina","Czarnecka","1944-02-13","K");

    -- Zawody
    INSERT INTO Zawody(nazwa, pensja_min, pensja_max) VALUES
    ("polityk", "8000", "20000"),
    ("nauczyciel", "3000", "6000"),
    ("lekarz", "9000", "30000"),
    ("informatyk", "6000", "25000");

    -- Procedura przydzielajaca zawody ludziom
    DELIMITER $$
    CREATE PROCEDURE hand_out_jobs()
    BEGIN
        DECLARE czlowiek int unsigned;
        DECLARE data_uro date;
        DECLARE plec_czl enum('K', 'M');
        DECLARE finished int DEFAULT 0;
        DECLARE zawod int;
        DECLARE pensja_val float;
        DECLARE pensja_minval float;
        DECLARE pensja_maxval float;
        DECLARE wiek int;

        DECLARE ludzie CURSOR FOR (SELECT czlowiek_id, data_urodzenia, plec FROM Ludzie);
        DECLARE CONTINUE HANDLER FOR NOT FOUND SET finished = 1;
        OPEN Ludzie;

        WHILE NOT finished DO
        FETCH ludzie INTO czlowiek, data_uro, plec_czl;
            IF NOT finished THEN
                SET wiek = DATEDIFF(CURDATE(), data_uro);
                -- Sprawdzamy czy osoba jest pelnoletnia
                IF (wiek >= 18 * 365) THEN
                    IF ((wiek > 65 * 365 AND plec_czl = 'M') OR (wiek > 60 * 365 AND plec_czl = 'K')) THEN
                        SET zawod = (SELECT zawod_id FROM Zawody AS z WHERE nazwa NOT LIKE "lekarz" ORDER BY RAND() LIMIT 1);
                    ELSE
                        SET zawod = (SELECT zawod_id FROM Zawody AS z ORDER BY RAND() LIMIT 1);
                    END IF;

                    SET pensja_minval = (SELECT pensja_min FROM Zawody AS z WHERE z.zawod_id LIKE zawod);
                    SET pensja_maxval = (SELECT pensja_max FROM Zawody AS z WHERE z.zawod_id LIKE zawod);
                    SET pensja_val = pensja_minval + (pensja_maxval - pensja_minval) * RAND();
                    INSERT INTO Pracownicy(czlowiek_id, zawod_id, pensja) VALUES (czlowiek, zawod, pensja_val);
                END IF;
            END IF;
        END WHILE;

        CLOSE Ludzie;
    END$$
    DELIMITER ;

2:
    -- Najpierw tworze indexy
    CREATE INDEX plec_oraz_imie ON Ludzie(plec, imie);
    CREATE INDEX pensja ON Pracownicy(pensja);

    1) SELECT * FROM Ludzie WHERE imie LIKE "A%" AND plec LIKE 'K';
    2) SELECT * FROM Ludzie WHERE plec = "K";
    3) SELECT * FROM Ludzie WHERE imie LIKE "K%";
    4) SELECT * FROM Ludzie,Pracownicy WHERE Ludzie.czlowiek_id LIKE Pracownicy.czlowiek_id AND pensja < 2000;
    5) SELECT * FROM Ludzie,Pracownicy,Zawody WHERE Pracownicy.czlowiek_id LIKE Ludzie.czlowiek_id AND Pracownicy.zawod_id LIKE Zawody.zawod_id AND Ludzie.plec LIKE 'M' AND Zawody.nazwa LIKE 'informatyk' AND Pracownicy.pensja < 10000;

    1,2,5 uzywaja indeksu plec_oraz_imie
    4,5 uzywaja indeksu pensja

    SHOW INDEX IN Ludzie;
    SHOW INDEX IN Pracownicy;
    W tabeli ludzie mamy zalozone indexy:
    - PRIMARY,
    - plec_oraz_imie,
    W tabeli pracownicy mamy zalozone indexy:
    - PRIMARY,
    - zawod_id,
    - pensja

4:
    -- Najpierw uzywamy PREPARE do stworzenia zapytania
    PREPARE nr_of_women FROM 'SELECT COUNT(*) FROM Ludzie, Pracownicy, Zawody
    WHERE Pracownicy.czlowiek_id LIKE Ludzie.czlowiek_id AND Pracownicy.zawod_id LIKE Zawody.zawod_id
    AND Zawody.nazwa LIKE ? AND Ludzie.plec LIKE "K"';

    -- Potem EXECUTE
    EXECUTE nr_of_women USING "polityk";
    EXECUTE nr_of_women USING "nauczyciel";

5:
    -- Backup
    mysqldump -u root -p firma > firma_back.sql
    -- Usuniecie bazy
    DROP DATABASE firma;
    -- Przywrocenie bazy z backupu
    mysql -u root -p praca < firma_back.sql

    Roznica miedzy backupem pelnym, a roznicowym jest taka, ze
    backup pelny tworzy kopie wszystkich danych zapisanych w bazie,
    a backup roznicowy zapisuje tylko dane ktore zostaly zmienione
    od ostatniego backupu
