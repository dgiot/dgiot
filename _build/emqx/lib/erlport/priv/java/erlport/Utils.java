package erlport;

import java.io.*;
import erlport.terms.*;

class Utils {

    // Java String -> Erlang Binary
    static Binary stringToBinary(String str) {
        return new Binary(str.getBytes());   
    }

    static String getStackTrace(Exception e) {
        StringWriter sw = null;
        PrintWriter pw = null;
        try {
            sw = new StringWriter();
            pw = new PrintWriter(sw);
            e.printStackTrace(pw);
            pw.flush();
            sw.flush();

        } catch (Exception e2) {
            e2.printStackTrace();
        } finally {
            if (sw != null) {
                try {
                    sw.close();
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
            }
            if (pw != null) {
                pw.close();
            }
        }
        return sw.toString();
    }

    static Object decode_opaque_object(Tuple t) throws Exception {

        if (t.get(1) instanceof Atom && ((Atom) t.get(1)).value.equals("java")) {

            byte[] bytes = ((Binary) t.get(2)).raw;

            ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(bytes);
            ObjectInputStream objectInputStream = new ObjectInputStream(byteArrayInputStream);

            return objectInputStream.readObject();
        }

        return t;
    }

    static Tuple encode_opaque_object(Object obj) throws Exception {
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
