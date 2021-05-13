package erlport.terms;

import java.util.*;
import java.lang.*;

public class Binary {

    public byte[] raw;

    public Binary(byte[] bytes) {
        this.raw = bytes;
    }

    public Binary(String str) {
        this.raw = str.getBytes();
    }

    @Override
    public String toString() {
        if (raw.length == 0 ) {
            return "Binary([])";
        }
        try {
            return String.format("Binary(\"%s\")", new String(raw));
        } catch (Exception e) {
            return String.format("Binary(%s)", Arrays.toString(raw));
        }
    }
}
