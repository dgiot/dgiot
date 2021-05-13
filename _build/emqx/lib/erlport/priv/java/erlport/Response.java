package erlport;

import java.util.*;
import java.lang.*;
import java.nio.*;
import java.math.*;

import erlport.terms.*;

class Response extends Object {

    // {'r', Id, Result}
    // {'e', Id, Error}
    // {'e', Error}
    
    String type;
    Integer id;
    Object result;

    public static Response success(Integer id, Object result) {
        return new Response("r", id, result);
    }

    public static Response failure(Integer id, Object error) {
        return new Response("e", id, error);
    }

    public static Response stop(Object error) {
        return new Response("e", error);
    }

    private Response(String type, Integer id, Object result) {
        this.type = type;
        this.id = id;
        this.result = result;
    }

    private Response(String type, Object result) {
        this.type = type;
        this.result = result;
    }

    public byte[] pack() throws Exception {
        if (id == null) {
            Tuple resp = new Tuple(2);
            resp.set(0, new Atom(type));
            resp.set(1, result);
            return pack_tag_terms(resp);
        } else {
            Tuple resp = new Tuple(3);
            resp.set(0, new Atom(type));
            resp.set(1, id);
            resp.set(2, result);
            return pack_tag_terms(resp);
        }
    }

    private byte[] pack_tag_terms(Object obj) throws Exception {
        //System.err.println(obj);

        // SMALL_INTEGER | INTEGER
        if (obj instanceof Integer) {
            Integer i = (Integer) obj;
            if (i < 256 && i >= 0) { // SMALL_INTEGER
                return new byte[]{97, i.byteValue()};
            } else {
                ByteBuffer bb = ByteBuffer.allocate(5);
                bb.put((byte) 98);
                bb.putInt(i);
                return bb.array();
            }
        }

        // SMALL_BIG | LARGE_BIG
        if (obj instanceof BigInteger) {
            BigInteger bi = (BigInteger) obj;
            Integer sign = 0;
            if (bi.signum() == -1) {
                sign = 1;
                bi = bi.abs();
            }
            byte[] src = bi.toByteArray();  // big-endian
            Integer len = src.length;

            byte[] dest = new byte[len];    // little-endian

            for(Integer i=0; i<len; i++) {
                dest[len-i-1] = src[i];
            }

            byte[] lenBytes = pack_unsigned(len, 4);
            byte[] signBytes = pack_unsigned(sign, 1);

            ByteBuffer bb = ByteBuffer.allocate(6+len);
            bb.put((byte) 111);
            bb.put(lenBytes);
            bb.put(signBytes);
            bb.put(dest);

            return bb.array();
        }

        // FLOAT ?? 31 String?

        // NEW_FLOAT (Double)
        if (obj instanceof Double) {
            ByteBuffer bb = ByteBuffer.allocate(9);
            bb.put((byte) 70);
            bb.putDouble((Double) obj);
            return bb.array();
        }

        // PORT | NEW_PORT
        if (obj instanceof Port) {
            Port port = (Port) obj;
            byte[] nodeBytes = pack_tag_terms(port.node);
            byte[] idBytes = pack_unsigned(port.id, 4);
            // Len??
            if (port.creation < 256 && port.creation >= 0) {
                ByteBuffer bb = ByteBuffer.allocate(nodeBytes.length + 6);
                bb.put((byte) 102);
                bb.put(nodeBytes);
                bb.put(idBytes);
                bb.put(pack_unsigned(port.creation, 1));
                return bb.array();
            } else {
                ByteBuffer bb = ByteBuffer.allocate(nodeBytes.length + 9);
                bb.put((byte) 89);
                bb.put(nodeBytes);
                bb.put(idBytes);
                bb.put(pack_unsigned(port.creation, 4));
                return bb.array();
            }
        }

        // PID | NEW_PID
        if (obj instanceof Pid) {
            Pid pid = (Pid) obj;
            byte[] nodeBytes = pack_tag_terms(pid.node);
            byte[] idBytes = pack_unsigned(pid.id, 4);
            byte[] serialBytes = pack_unsigned(pid.serial, 4);
            // Len??
            if (pid.creation < 256 && pid.creation >= 0) {
                ByteBuffer bb = ByteBuffer.allocate(nodeBytes.length + 10);
                bb.put((byte) 103);
                bb.put(nodeBytes);
                bb.put(idBytes);
                bb.put(serialBytes);
                bb.put(pack_unsigned(pid.creation, 1));
                return bb.array();
            } else {
                ByteBuffer bb = ByteBuffer.allocate(nodeBytes.length + 13);
                bb.put((byte) 88);
                bb.put(nodeBytes);
                bb.put(idBytes);
                bb.put(pack_unsigned(pid.creation, 4));
                return bb.array();
            }
        }

        // TUPLE | LARGE_TUPLE
        if (obj instanceof Tuple) {
            Tuple tuple = (Tuple) obj;
            ArrayList<byte[]> temp = new ArrayList<byte[]>();
            Integer size = 0;
            for(Object e: tuple.elements) {
                byte[] eBytes = pack_tag_terms(e);
                temp.add(eBytes);
                size += eBytes.length;
            }

            ByteBuffer bb;
            if (temp.size() < 256 && temp.size() >= 0) {
                bb = ByteBuffer.allocate(2 + size);
                bb.put((byte) 104);
                bb.put(pack_unsigned(temp.size(), 1));
            } else {
                bb = ByteBuffer.allocate(5 + size);
                bb.put((byte) 105);
                bb.put(pack_unsigned(temp.size(), 4));
            }
            for(byte[] eBytes: temp) {
                bb.put(eBytes);
            }
            return bb.array();
        }

        // Map
        if (obj instanceof Map) {
            Map<Object, Object> map = (Map<Object, Object>) obj;
            ArrayList<byte[]> temp = new ArrayList<byte[]>();
            Integer size = 0;
            for (Map.Entry<Object, Object> entry: map.entrySet()) {
                byte[] keyBytes = pack_tag_terms(entry.getKey());
                byte[] valBytes = pack_tag_terms(entry.getValue());
                temp.add(keyBytes);
                temp.add(valBytes);
                size = size + keyBytes.length + valBytes.length;
            }

            ByteBuffer bb = ByteBuffer.allocate(5 + size);
            bb.put((byte) 116);
            bb.put(pack_unsigned(temp.size(), 4));
            for(byte[] eBytes: temp) {
                bb.put(eBytes);
            }
            return bb.array();
        }

        // NIL | LIST (List)
        if (obj instanceof List) {
            List list = (List) obj;
            if (list.size() == 0) {
                return new byte[]{ (byte) 106, (byte) 106};
            } else {
                ArrayList<byte[]> temp = new ArrayList<byte[]>();
                Integer size = 0;
                for(Object e: list) {
                    byte[] eBytes = pack_tag_terms(e);
                    temp.add(eBytes);
                    size += eBytes.length;
                }
                ByteBuffer bb = ByteBuffer.allocate(6 + size);
                bb.put((byte) 108);
                bb.put(pack_unsigned(temp.size(), 4));
                for(byte[] eBytes: temp) {
                    bb.put(eBytes);
                }
                bb.put((byte) 106);
                return bb.array();
            }
        }

        // STRING (String)
        if (obj instanceof String) {
            String str = (String) obj;

            byte[] strBytes = str.getBytes();

            ByteBuffer bb = ByteBuffer.allocate(3 + strBytes.length);
            bb.put( (byte) 107);
            bb.put(pack_unsigned(strBytes.length, 2));
            bb.put(strBytes);

            return bb.array();
        }

        // BINARY (Binary)
        if (obj instanceof Binary) {
            Binary bin = (Binary) obj;
        
            ByteBuffer bb = ByteBuffer.allocate(5 + bin.raw.length);
            bb.put( (byte) 109);
            bb.put(pack_unsigned(bin.raw.length, 4));
            bb.put(bin.raw);

            return bb.array();
        }

        // ATOM | ATOM_UTF8             (Atom: len=2)
        // SMALL_ATOM | SMALL_ATOM_UTF8 (Atom: len=1)
        //
        //  XXX: OUTPOUT ==>>>>> ATOM_UTF8 and SMALL_ATOM_UTF8
        if (obj instanceof Atom) {
            Atom atom = (Atom) obj;

            ByteBuffer bb;
            byte[] strBytes = atom.value.getBytes();

            if (strBytes.length < 256 && strBytes.length >= 0) { // SMALL
                bb = ByteBuffer.allocate(2+strBytes.length);
                bb.put( (byte) 119);
                bb.put(pack_unsigned(strBytes.length, 1));
            } else {
                bb = ByteBuffer.allocate(3+strBytes.length);
                bb.put( (byte) 118);
                bb.put(pack_unsigned(strBytes.length, 2));
            }

            bb.put(strBytes);

            return bb.array();
        }

        // REFERENCE (Reference)
        if (obj instanceof Reference) {
            // TODO:
        }

        // Directly types

        // Boolean => Atom
        if (obj instanceof Boolean) {
            Boolean b = (Boolean) obj;
            if (b) {
                return pack_tag_terms(new Atom("true"));
            } else {
                return pack_tag_terms(new Atom("false"));
            }
        }

        // OpaqueObject => {'$erlport.opaque', java, Bytes}
        return pack_tag_terms(Utils.encode_opaque_object(obj));
    }

    // Value: 0~2^31-1;
    private byte[] pack_unsigned(Integer value, Integer len) {
        ByteBuffer bb = ByteBuffer.allocate(len);
        for(Integer i=0; i<len; i++) {
            bb.put( (byte) (value >> (8*(len-i-1)) & 0xff) );
        }
        return bb.array();
    }

    private byte[] pack_unsigned(Long value, Integer len) {
        ByteBuffer bb = ByteBuffer.allocate(len);
        for(Integer i=0; i<len; i++) {
            bb.put( (byte) (value >> (8*(len-i-1)) & 0xff) );
        }
        return bb.array();
    }

}
