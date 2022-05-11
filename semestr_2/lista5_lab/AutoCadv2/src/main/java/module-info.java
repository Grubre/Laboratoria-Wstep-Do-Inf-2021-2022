module com.example.autocadv2 {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.example.autocadv2 to javafx.fxml;
    exports com.example.autocadv2;
}