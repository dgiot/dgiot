package com.erlport.erlang.term;

import java.util.Arrays;

public class Tuple {

    public Object[] elements;

    public Tuple(Integer n) {
        elements = new Object[n];
    }

    public static Tuple two(Object first, Object second) {
        Tuple t = new Tuple(2);
        t.set(0, first);
        t.set(1, second);
        return t;
    }

    public static Tuple three(Object first, Object second, Object third) {
        Tuple t = new Tuple(3);
        t.set(0, first);
        t.set(1, second);
        t.set(2, third);
        return t;
    }

    public static Tuple four(Object first, Object second, Object third, Object fourth) {
        Tuple t = new Tuple(4);
        t.set(0, first);
        t.set(1, second);
        t.set(2, third);
        t.set(3, fourth);
        return t;
    }

    public void set(Integer pos, Object e) {
        elements[pos] = e;
    }

    public Object get(Integer pos) {
        return elements[pos];
    }

    public Integer length() {
        return elements.length;
    }

    @Override
    public String toString() {
        return "Tuple{" +
                "elements=" + Arrays.toString(elements) +
                '}';
    }
}
