1:
    CREATE DATABASE `db-aparaty`;
    CREATE USER '268493'@'localhost' IDENTIFIED BY 'jakub493';
    GRANT SELECT, INSERT, UPDATE on *.* TO '268493'@'localhost';
2:
    CREATE TABLE Matryca (
        ID int unsigned NOT NULL AUTO_INCREMENT PRIMARY KEY,
        przekatna decimal(4,2) NOT NULL,
        rozdzielczosc decimal(3,1) NOT NULL,
        typ varchar(10) NOT NULL
    );
    ALTER TABLE Matryca AUTO_INCREMENT=100;
    CREATE TABLE Obiektyw (
        ID int unsigned NOT NULL AUTO_INCREMENT PRIMARY KEY,
        model varchar(30) NOT NULL,
        minPrzeslona float NOT NULL,
        maxPrzeslona float NOT NULL
        CHECK (minPrzeslona < maxPrzeslona)
    );
    CREATE TABLE Producent (
        ID int unsigned NOT NULL AUTO_INCREMENT PRIMARY KEY,
        nazwa varchar(50),
        kraj varchar(20)
    );
    CREATE TABLE Aparat (
        model varchar(30) NOT NULL PRIMARY KEY,
        producent int unsigned NOT NULL,
        matryca int unsigned NOT NULL,
        obiektyw int unsigned NOT NULL,
        typ enum('kompaktowy', 'lustrzanka', 'profesjonalny', 'inny' ) NOT NULL,
        FOREIGN KEY (producent) REFERENCES Producent(ID),
        FOREIGN KEY (matryca) REFERENCES Matryca(ID),
        FOREIGN KEY (obiektyw) REFERENCES Obiektyw(ID)
    );

3:
    INSERT INTO Producent(nazwa, kraj) VALUES
        ('Comarch','Polska'),
        ('Nokia','Finlandia'),
        ('Lego','Dania'),
        ('Canon','Polska'),
        ('Sony','Japonia'),
        ('Samsung','Korea Poludniowa'),
        ('HM','Szwecja'),
        ('Rebook','Polska'),
        ('4F','Polska'),
        ('Toshiba','Japonia'),
        ('Wuhan_Labs','Chiny'),
        ('Chonghinq_manufactur','Chiny'),
        ('Beijing_amazon','Chiny'),
        ('Shanghai_corporate','Chiny'),
        ('Beijing_manufactur','Chiny'),
        ('Amazon', 'Chinska Republika Ludowa'),
        ('Meta', 'Zuckerberg Commonwealth');
    INSERT INTO Obiektyw(model, minPrzeslona, maxPrzeslona) VALUES
        ('Model0', 10, 11),
        ('Model1', 11, 12),
        ('Model2', 12, 13),
        ('Model3', 13, 14),
        ('Model4', 14, 15),
        ('Model5', 15, 16),
        ('Model6', 16, 17),
        ('Model7', 17, 18),
        ('Model8', 18, 19),
        ('Model9', 19, 20),
        ('Model10', 20, 21),
        ('Model11', 21, 22),
        ('Model12', 22, 23),
        ('Model13', 23, 24),
        ('Model14', 24, 25),
        ('Model15', 25, 24),
        ('Model17', 75, 24);
    INSERT INTO Matryca(przekatna, rozdzielczosc, typ) VALUES
        (15.15, 23.1, "Typ1"),
        (16.16, 24.2, "Typ2"),
        (17.17, 25.3, "Typ3"),
        (18.18, 26.4, "Typ4"),
        (19.19, 27.5, "Typ5"),
        (20.20, 28.6, "Typ6"),
        (21.21, 29.7, "Typ7"),
        (22.22, 30.8, "Typ8"),
        (23.23, 31.9, "Typ9"),
        (24.24, 32.1, "Typ10"),
        (25.25, 33.1, "Typ11"),
        (26.26, 34.1, "Typ12"),
        (27.27, 35.1, "Typ13"),
        (28.28, 36.1, "Typ14"),
        (29.29, 37.1, "Typ15"),
        (NULL, NULL, NULL),
        (NULL, NULL, NULL);
    INSERT INTO Aparat(model, producent, matryca, obiektyw, typ) VALUES
        ("Model0", 15, 100, 1, 'kompaktowy'),
        ("Model1", 15, 101, 3, 'lustrzanka'),
        ("Model2", 15, 101, 2, 'kompaktowy'),
        ("Model3", 15, 101, 4, 'lustrzanka'),
        ("Model4", 15, 104, 11, 'profesjonalny'),
        ("Model5", 12, 103, 12, 'kompaktowy'),
        ("Model6", 8, 103, 10, 'inny'),
        ("Model7", 12, 103, 9, 'profesjonalny'),
        ("Model8", 13, 105, 5, 'kompaktowy'),
        ("Model9", 2, 106, 4, 'kompaktowy'),
        ("Model10", 3, 107, 8, 'lustrzanka'),
        ("Model11", 2, 110, 3, 'kompaktowy'),
        ("Model12", 3, 104, 3, 'profesjonalny'),
        ("Model13", 1, 103, 4, 'kompaktowy'),
        ("Model14", 1, 102, 5, 'inny'),
        (NULL, NULL, NULL),
        (NULL, NULL, NULL);
4:
    CREATE PROCEDURE gen_aparat()
        BEGIN
        declare model varchar(30);
        declare producent int unsigned;
        declare matryca int unsigned;
        declare obiektyw int unsigned;
        declare typ enum('kompaktowy', 'lustrzanka', 'profesjonalny', 'inny' );

        declare producent_cnt int unsigned;
        declare matryca_cnt int unsigned;
        declare obiektyw_cnt int unsigned;

        declare i INT default 0;

        declare rand_var INT default 0;

        SET producent_cnt = (SELECT COUNT(*) FROM Producent);
        SET matryca_cnt = (SELECT COUNT(*) FROM Matryca);
        SET obiektyw_cnt = (SELECT COUNT(*) FROM Obiektyw);

        SET i = 1;

        WHILE i <= 100 DO
            SET rand_var = (SELECT FLOOR(RAND()*(5000-1+250))+250);
            SET model = concat("AparatModel",i,"x", rand_var);

            SET producent = (SELECT ID FROM Producent ORDER BY Rand() LIMIT 1);
            SET matryca = (SELECT ID FROM Matryca ORDER BY Rand() LIMIT 1);
            SET obiektyw = (SELECT ID FROM Obiektyw ORDER BY Rand() LIMIT 1);

            SET rand_var = (SELECT FLOOR(RAND()*(4-1+1))+1);

            SET typ = (CASE
                WHEN rand_var LIKE 1 THEN 'kompaktowy'
                WHEN rand_var LIKE 2 THEN 'lustrzanka'
                WHEN rand_var LIKE 3 THEN 'profesjonalny'
                ELSE 'inny'
            END);


            INSERT INTO Aparat(model, producent, matryca, obiektyw, typ) VALUES
                (model, producent, matryca, obiektyw, typ);
            
            SET i = i + 1;
        END WHILE;
    END;
5:
    CREATE PROCEDURE get_max_przekatna(IN In_ID int unsigned, OUT output varchar(30))
    BEGIN
        SELECT Model INTO output FROM Aparat, Matryca WHERE Aparat.matryca=Matryca.ID AND Producent=In_ID ORDER BY przekatna DESC LIMIT 1;
    END;
6:
7:
    CREATE FUNCTION get_amount_of_matryca(In_ID int unsigned)
    RETURNS int unsigned DETERMINISTIC
    RETURN (SELECT COUNT(*) FROM Aparat WHERE Matryca=In_ID);
8:
9:
    CREATE VIEW view1 AS SELECT Aparat.model, Producent.nazwa, Matryca.przekatna, Matryca.rozdzielczosc,
    Obiektyw.minPrzeslona, Obiektyw.maxPrzeslona FROM Aparat, Producent, Matryca, Obiektyw WHERE
    Aparat.producent=Producent.ID AND Aparat.matryca=Matryca.ID AND Aparat.obiektyw=Obiektyw.ID
    AND Producent.kraj NOT LIKE "Chiny";
10:
    CREATE VIEW view2 AS SELECT Producent.nazwa, Producent.kraj, Aparat.model FROM Aparat,Producent WHERE
    Aparat.producent=Producent.ID;
    DELETE FROM Aparat WHERE Aparat.producent IN (SELECT ID FROM Producent WHERE kraj = "Chiny");
11:
    
