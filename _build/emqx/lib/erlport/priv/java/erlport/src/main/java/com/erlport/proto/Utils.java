package com.erlport.proto;

import com.erlport.erlang.term.Atom;
import com.erlport.erlang.term.Binary;
import com.erlport.erlang.term.OpaqueObject;
import com.erlport.erlang.term.Tuple;

import java.io.*;

public class Utils {

    // Java String -> Erlang Binary
    public static Binary stringToBinary(String str) {
        return new Binary(str.getBytes());
    }

    public static String getStackTrace(Exception e) {
        StringWriter sw = new StringWriter();
        try (PrintWriter pw = new PrintWriter(sw)) {
            e.printStackTrace(pw);
            pw.flush();
            sw.flush();

        } catch (Exception e2) {
            e2.printStackTrace();
        }
        return sw.toString();
    }

    static Object decodeOpaqueObject(Tuple t) throws Exception {
        if (t.get(1) instanceof Atom) {
            Atom lang = (Atom) t.get(1);
            if (lang.value.equals("java")) {
                byte[] bytes = ((Binary) t.get(2)).raw;

                ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(bytes);
                ObjectInputStream objectInputStream = new ObjectInputStream(byteArrayInputStream);

                return objectInputStream.readObject();
            } else if (lang.value.equals("erlang")) {
                return new OpaqueObject(lang, (Binary) t.get(2));
            } else {
                throw new Exception("not supported opaque object type: " + lang.value);
            }
        } else {
            return new Exception("invalid opaque object bytes");
        }
    }

    public static Tuple encodeOpaqueObject(Object obj) throws Exception {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream);

        objectOutputStream.writeObject(obj);
        objectOutputStream.flush();

        return Tuple.three(
                new Atom("$erlport.opaque"),
                new Atom("java"),
                new Binary(byteArrayOutputStream.toByteArray()));
    }
}
