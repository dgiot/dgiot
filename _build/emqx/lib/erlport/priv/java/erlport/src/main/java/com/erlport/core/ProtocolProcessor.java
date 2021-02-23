package com.erlport.core;

import com.erlport.erlang.term.*;
import com.erlport.proto.Utils;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author wangwenhai
 * @date 2020/7/15
 */
public class ProtocolProcessor {

    //--------------------------------------------------------------------------------
    public static byte[] serialize(Object obj) throws Exception {

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
            byte[] nodeBytes = serialize(port.node);
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
            byte[] nodeBytes = serialize(pid.node);
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
            int size = 0;
            for (Object e : tuple.elements) {
                if (e != null) {
                    byte[] eBytes = serialize(e);
                    temp.add(eBytes);
                    size += eBytes.length;
                }

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
            Map<Object, Object> map = (Map) obj;
            ArrayList<byte[]> temp = new ArrayList<>();
            int size = 0;
            for (Map.Entry<Object, Object> entry : map.entrySet()) {
                byte[] keyBytes = serialize(entry.getKey());
                byte[] valBytes = serialize(entry.getValue());
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
                ArrayList<byte[]> temp = new ArrayList<>();
                int size = 0;
                for (Object e : list) {
                    byte[] eBytes = serialize(e);
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
                    byte[] eBytes = serialize(e);
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
        //
        //  XXX: OUTPOUT ==>>>>> ATOM_UTF8 and SMALL_ATOM_UTF8
        if (obj instanceof Atom) {
            Atom atom = (Atom) obj;

            ByteBuffer bb;
            byte[] strBytes = atom.value.getBytes();

            if (strBytes.length < 256) { // SMALL
                bb = ByteBuffer.allocate(2 + strBytes.length);
                bb.put((byte) 119);
                bb.put(pack_unsigned(strBytes.length, 1));
            } else {
                bb = ByteBuffer.allocate(3 + strBytes.length);
                bb.put((byte) 118);
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
            return b ? serialize(new Atom("true")) : serialize(new Atom("false"));
        }

        // OpaqueObject => {'$erlport.opaque', java, Bytes}
        return serialize(Utils.encodeOpaqueObject(obj));
    }

    // Value: 0~2^31-1;
    private static byte[] pack_unsigned(Integer value, Integer len) {
        ByteBuffer bb = ByteBuffer.allocate(len);
        for (Integer i = 0; i < len; i++) {
            bb.put((byte) (value >> (8 * (len - i - 1)) & 0xff));
        }
        return bb.array();
    }

    private static byte[] pack_unsigned(Long value, Integer len) {
        ByteBuffer bb = ByteBuffer.allocate(len);
        for (Integer i = 0; i < len; i++) {
            bb.put((byte) (value >> (8 * (len - i - 1)) & 0xff));
        }
        return bb.array();
    }

}
