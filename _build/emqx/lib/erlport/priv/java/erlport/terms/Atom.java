package erlport.terms;

import java.util.*;
import java.lang.*;

public class Atom extends Object {

    public String value;

    public Atom(String v) {
        this.value = v;
    }

    @Override
    public String toString() {
        return String.format("Atom(\"%s\")", value);
    }
}
