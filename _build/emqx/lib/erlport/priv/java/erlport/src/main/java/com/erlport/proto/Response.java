package com.erlport.proto;

import com.erlport.erlang.term.*;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Type:
 * Response: {'r', Id, Result}
 * -Error : {'e', Id, Error}
 * -Error : {'e', Error}
 */
public class Response {

    private String type;
    private Integer responseId;
    private Object result;

    private Response(String type, Integer responseId, Object result) {
        this.type = type;
        this.responseId = responseId;
        this.result = result;
    }

    private Response(String type, Object result) {
        this.type = type;
        this.result = result;
    }

    public static Response of(Integer responseId, Object result) {
        return new Response("r", responseId, result);
    }

    public static Response success(Integer responseId, Object result) {
        return new Response("r", responseId, result);
    }

    public static Response failure(Integer responseId, Object error) {
        return new Response("e", responseId, error);
    }

    public static Response stop(Object error) {
        return new Response("e", error);
    }

    byte[] pack() throws Exception {
        if (responseId == null) {
            Tuple resp = new Tuple(2);
            resp.set(0, new Atom(type));
            resp.set(1, result);
            return pack_tag_terms(resp);
        } else {
            Tuple resp = new Tuple(3);
            resp.set(0, new Atom(type));
            resp.set(1, responseId);
            resp.set(2, result);
            return pack_tag_terms(resp);
        }
    }

    private byte[] pack_tag_terms(Object obj) throws Exception {
        // OpaqueObject
        if (obj instanceof OpaqueObject) {
            OpaqueObject oo = (OpaqueObject) obj;
            if (oo.lang.value.equals("erlang")) {
                return oo.value.raw;
            } else {
                throw new Exception("not support type");
            }
        }

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
            int sign = 0;
            if (bi.signum() == -1) {
                sign = 1;
                bi = bi.abs();
            }
            byte[] src = bi.toByteArray();  // big-endian
            Integer len = src.length;

            byte[] dest = new byte[len];    // little-endian

            for (Integer i = 0; i < len; i++) {
                dest[len - i - 1] = src[i];
            }

            byte[] lenBytes = pack_unsigned(len, 4);
            byte[] signBytes = pack_unsigned(sign, 1);

            ByteBuffer bb = ByteBuffer.allocate(6 + len);
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
            ArrayList<byte[]> temp = new ArrayList<>();
            int size = 0;
            for (Object e : tuple.elements) {
                byte[] eBytes = pack_tag_terms(e);
                temp.add(eBytes);
                size += eBytes.length;
            }

            ByteBuffer bb;
            if (temp.size() < 256 && temp.size() >= 1) {
                bb = ByteBuffer.allocate(2 + size);
                bb.put((byte) 104);
                bb.put(pack_unsigned(temp.size(), 1));
            } else {
                bb = ByteBuffer.allocate(5 + size);
                bb.put((byte) 105);
                bb.put(pack_unsigned(temp.size(), 4));
            }
            for (byte[] eBytes : temp) {
                bb.put(eBytes);
            }
            return bb.array();
        }

        // Map
        if (obj instanceof Map) {
            Map<Object, Object> map = ( Map<Object, Object>) obj;
            ArrayList<byte[]> temp = new ArrayList<>();
            int size = 0;
            for (Map.Entry  entry : map.entrySet()) {
                byte[] keyBytes = pack_tag_terms(entry.getKey());
                byte[] valBytes = pack_tag_terms(entry.getValue());
                temp.add(keyBytes);
                temp.add(valBytes);
                size = size + keyBytes.length + valBytes.length;
            }

            ByteBuffer bb = ByteBuffer.allocate(5 + size);
            bb.put((byte) 116);
            bb.put(pack_unsigned(temp.size(), 4));
            for (byte[] eBytes : temp) {
                bb.put(eBytes);
            }
            return bb.array();
        }

        // NIL | LIST (List)
        if (obj instanceof List) {
            List list = (List) obj;
            if (list.size() == 0) {
                return new byte[]{(byte) 106, (byte) 106};
            } else {
                ArrayList<byte[]> temp = new ArrayList<byte[]>();
                int size = 0;
                for (Object e : list) {
                    byte[] eBytes = pack_tag_terms(e);
                    temp.add(eBytes);
                    size += eBytes.length;
                }
                ByteBuffer bb = ByteBuffer.allocate(6 + size);
                bb.put((byte) 108);
                bb.put(pack_unsigned(temp.size(), 4));
                for (byte[] eBytes : temp) {
                    bb.put(eBytes);
                }
                bb.put((byte) 106);
                return bb.array();
            }
        }
        // Objects[]
        if (obj instanceof Object[]) {
            Object[] list = (Object[]) obj;
            if (list.length == 0) {
                return new byte[]{(byte) 106, (byte) 106};
            } else {
                ArrayList<byte[]> temp = new ArrayList<>();
                int size = 0;
                for (Object e : list) {
                    byte[] eBytes = pack_tag_terms(e);
                    temp.add(eBytes);
                    size += eBytes.length;
                }
                ByteBuffer bb = ByteBuffer.allocate(6 + size);
                bb.put((byte) 108);
                bb.put(pack_unsigned(temp.size(), 4));
                for (byte[] eBytes : temp) {
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
            bb.put((byte) 107);
            bb.put(pack_unsigned(strBytes.length, 2));
            bb.put(strBytes);

            return bb.array();
        }

        // BINARY (Binary)
        if (obj instanceof Binary) {
            Binary bin = (Binary) obj;

            ByteBuffer bb = ByteBuffer.allocate(5 + bin.raw.length);
            bb.put((byte) 109);
            bb.put(pack_unsigned(bin.raw.length, 4));
            bb.put(bin.raw);

            return bb.array();
        }

        // ATOM | ATOM_UTF8             (Atom: len=2)
        // SMALL_ATOM | SMALL_ATOM_UTF8 (Atom: len=1)
        if (obj instanceof Atom) {
            Atom atom = (Atom) obj;

            ByteBuffer bb;
            byte[] strBytes = atom.value.getBytes(atom.coder);

            if (atom.coder.equals("UTF-8")) {
                if (strBytes.length < 256) { // SMALL
                    bb = ByteBuffer.allocate(2 + strBytes.length);
                    bb.put((byte) 119);
                    bb.put(pack_unsigned(strBytes.length, 1));
                } else {
                    bb = ByteBuffer.allocate(3 + strBytes.length);
                    bb.put((byte) 118);
                    bb.put(pack_unsigned(strBytes.length, 2));
                }
            } else {
                if (strBytes.length < 256) { // SMALL
                    bb = ByteBuffer.allocate(2 + strBytes.length);
                    bb.put((byte) 115);
                    bb.put(pack_unsigned(strBytes.length, 1));
                } else {
                    bb = ByteBuffer.allocate(3 + strBytes.length);
                    bb.put((byte) 100);
                    bb.put(pack_unsigned(strBytes.length, 2));
                }
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
            return b ? pack_tag_terms(new Atom("true")) : pack_tag_terms(new Atom("false"));
        }

        // OpaqueObject => {'$erlport.opaque', java, Bytes}
        return pack_tag_terms(Utils.encodeOpaqueObject(obj));
    }

    // Value: 0~2^31-1;
    private byte[] pack_unsigned(Integer value, Integer len) {
        ByteBuffer bb = ByteBuffer.allocate(len);
        for (Integer i = 0; i < len; i++) {
            bb.put((byte) (value >> (8 * (len - i - 1)) & 0xff));
        }
        return bb.array();
    }

    private byte[] pack_unsigned(Long value, Integer len) {
        ByteBuffer bb = ByteBuffer.allocate(len);
        for (Integer i = 0; i < len; i++) {
            bb.put((byte) (value >> (8 * (len - i - 1)) & 0xff));
        }
        return bb.array();
    }

}
