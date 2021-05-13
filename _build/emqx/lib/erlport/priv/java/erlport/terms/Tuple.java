package erlport.terms;

import java.util.*;
import java.lang.*;

public class Tuple extends Object {

    public Object[] elements;

    public static Tuple forElements(Object[] l) {
        Tuple t = new Tuple(l.length);
        t.elements = l;
        return t;
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

    public Tuple(Integer n) {
        elements = new Object[n];
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

        if (elements.length == 0) {
            return "Tuple({})";
        }

        StringBuilder sb = new StringBuilder("Tuple({");
        for(Object e: elements) {
            if (e != null) {
                sb.append(e.toString() + ", ");
            } else {
                sb.append("null");
            }
        }
        sb.delete(sb.length()-2, sb.length());
        sb.append("})");
        return sb.toString();
    }
}
