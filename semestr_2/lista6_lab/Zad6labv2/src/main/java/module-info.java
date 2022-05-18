module com.example.zad6labv2 {
    requires javafx.controls;
    requires javafx.fxml;

    opens com.example.zad6labv2 to javafx.fxml;
    exports com.example.zad6labv2;
}