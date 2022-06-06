module com.example.binary_tree_v2 {
    requires javafx.controls;
    requires javafx.fxml;


    opens com.example.binary_tree_v2 to javafx.fxml;
    exports com.example.binary_tree_v2;
}