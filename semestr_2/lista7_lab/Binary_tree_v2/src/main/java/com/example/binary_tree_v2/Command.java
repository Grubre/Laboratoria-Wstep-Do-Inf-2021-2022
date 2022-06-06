package com.example.binary_tree_v2;

import java.io.Serializable;

/**
 * An object that we send from the client to the server containing the appropriate command
 * and optionally a value (when we insert, search or delete)
 * @param <T> Type of value sent to the server (String, Integer or Double)
 */
public record Command<T extends Comparable & Serializable>(String command, T value) implements Serializable {
}
