package com.erlport.proto;

import com.erlport.erlang.term.*;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * Type:
 * -Call : {'C', Id, Module, Function, Args}
 * -Message: {'M', Terms}
 */
public class Request {

    public Object rawTerm;
    // ----- Properties
    public RequestType type;
    public Integer requestId;
    // ----- End
    public Atom classname;
    public Atom methodName;
    public Object[] args;
    private Object message;
    private Object context;
    // ----- ParseState
    private Integer pos;
    private byte[] bytes;
    // ----- End


    Request(byte[] bytes) throws Exception {
        this.pos = 0;
        this.bytes = bytes;
        if (bytes[pos++] == -125) { // 131 ===> Term
            rawTerm = parse_tag_terms();
            Tuple request = (Tuple) rawTerm;
            if (request != null) {
                Atom type = (Atom) request.get(0);
                switch (type.value) {
                    case "C":
                        this.type = RequestType.CALL;
                        this.requestId = (Integer) request.get(1);
                        this.classname = (Atom) request.get(2);
                        this.methodName = (Atom) request.get(3);
                        this.args = ((List) request.get(4)).toArray();
                        break;
                    case "M":
                        this.type = RequestType.MESSAGE;
                        this.message = request.get(1);
                        break;
                    case "r":
                        this.type = RequestType.RESULT;
                        this.message = request.get(1);
                        break;
                    case "e":
                        this.type = RequestType.ERROR;
                        this.requestId = (Integer) request.get(1);
                        break;
                }
            }
        } else if (bytes[0] == 80) { // 80 ===> Compressed
            // TODO:
        }
    }

    private Object parse_tag_terms() throws Exception {
        int tag = parse_unsigned(1).intValue() & 0xff;
        //System.err.printf("Parsing tag %d...\n", tag);

        // SMALL_INTEGER
        if (tag == 97) {
            return parse_unsigned(1).intValue();
        }

        // INTEGER
        if (tag == 98) {
            return parse_signed_int32();
        }

        // FLOAT
        if (tag == 99) {
            return parse_float_str(31);
        }

        // NEW_FLOAT
        if (tag == 70) {
            return parse_ieee_float(8);
        }

        // PORT | NEW_PORT
        if (tag == 102 || tag == 89) {
            Atom node = (Atom) parse_tag_terms();
            Long id = parse_unsigned(4);
            Long creation = parse_unsigned(tag == 102 ? 1 : 4);
            return new Port(node, id, creation);
        }

        // PID | NEW_PID
        if (tag == 103 || tag == 88) {
            Atom node = (Atom) parse_tag_terms();
            Long id = parse_unsigned(4);
            Long serial = parse_unsigned(4);
            Long creation = parse_unsigned(tag == 103 ? 1 : 4);
            return new Pid(node, id, serial, creation);
        }

        // SMALL_TUPLE | LARGE_TUPLE
        if (tag == 104 || tag == 105) {
            Integer len = parse_unsigned(tag == 104 ? 1 : 4).intValue();
            Tuple t = new Tuple(len);
            for (Integer i = 0; i < len; i++) {
                t.set(i, parse_tag_terms());
            }

            if (t.elements.length == 3 && t.get(0) instanceof Atom && ((Atom) t.get(0)).value.equals("$erlport.opaque")) {
                return Utils.decodeOpaqueObject(t);
            }
            return t;
        }

        // MAP
        if (tag == 116) {
            Integer len = parse_unsigned(4).intValue();

            Map map = new HashMap<>();
            for (Integer i = 0; i < len; i++) {
                map.put(parse_tag_terms(), parse_tag_terms());
            }

            return map;
        }

        // NIL
        if (tag == 106) {
            return new ArrayList<>();
        }

        // STRING
        if (tag == 107) {
            Integer len = parse_unsigned(2).intValue();
            return parse_string(len);
        }

        // LIST
        if (tag == 108) {
            Long len = parse_unsigned(4);
            List<Object> list = new ArrayList<Object>();
            for (Long i = 0L; i < len; i++) {
                list.add(parse_tag_terms());
            }

            // drop tails
            parse_tag_terms();

            return list;
        }

        // BINARY
        if (tag == 109) {
            Integer len = parse_unsigned(4).intValue();
            return parse_binary(len);
        }

        // SMALL_BIG | LARGE_BIG
        if (tag == 110 || tag == 111) {
            Integer len = parse_unsigned(tag == 110 ? 1 : 4).intValue();
            return parse_big_integer(len);
        }

        // ATOM_UTF8 | ATOM  ( UTF-8 | ISO-8859-1)
        if (tag == 118 || tag == 100) {
            Integer len = parse_unsigned(2).intValue();
            String coder;
            String atomStr;
            if (tag == 118) {
                coder = "UTF-8";
                atomStr = parse_string(len);
            } else {
                coder = "ISO-8859-1";
                atomStr = parse_latin_1_string(len);
            }
            if (atomStr.equals("true") || atomStr.equals("false")) {
                return Boolean.valueOf(atomStr);
            }
            return new Atom(atomStr, coder);
        }

        // SMALL_ATOM_UTF8 | SMALL_ATOM (UTF-8 | ISO-8859-1)
        if (tag == 119 || tag == 115) {
            Integer len = parse_unsigned(1).intValue();
            String coder;
            String atomStr;
            if (tag == 119) {
                coder = "UTF-8";
                atomStr = parse_string(len);
            } else {
                coder = "ISO-8859-1";
                atomStr = parse_latin_1_string(len);
            }
            return new Atom(atomStr, coder);
        }

        // REFERENCE
        if (tag == 101) {
            Atom node = (Atom) parse_tag_terms();
            Long id = parse_unsigned(4);
            Long creation = parse_unsigned(1);
            return new Reference(node, id, creation);
        }

        // TODO:
        // NEW_REFERENCE 114
        // NEWER_REFERENCE 90
        // FUN 117
        // NEW_FUN 112
        // EXPORT 113
        // BIT_BINARY 77
        //

        return null;
    }

    // Parse signed integer; bigg-endian; just for 4 bytes
    // Byte in java have symbol problem, let a byte &0xFF can resolve
    private Integer parse_signed_int32() {
        int c = 0;
        for (int i = 1; i <= 4; i++, pos++) {
            c = c | ((bytes[pos] & 0xff) << ((4 - i) * 8));
        }
        return c;
    }

    // Parse unsigned integer; bigg-endia; max allowed 4 bytes
    private Long parse_unsigned(Integer len) {
        long c = 0L;
        for (int i = 1; i <= len; i++, pos++) {
            c = c | ((bytes[pos] & 0xff) << ((len - i) * 8));
        }
        return (c);
    }

    private String parse_float_str(Integer len) {
        String s = new String(bytes, pos, len);
        pos = pos + len;
        return s;
    }

    private double parse_ieee_float(Integer len) {
        double d = ByteBuffer.wrap(bytes, pos, len).getDouble();
        pos = pos + len;
        return d;
    }

    private Atom parse_atom_utf8(Integer len) {
        String s = new String(bytes, pos, len);
        pos = pos + len;
        return new Atom(s);
    }

    private Binary parse_binary(Integer len) {
        Binary b = new Binary(Arrays.copyOfRange(bytes, pos, pos + len));
        pos = pos + len;
        return b;
    }

    private BigInteger parse_big_integer(Integer len) {

        int sign = parse_unsigned(1).intValue();

        sign = sign == 0 ? 1 : -1;

        byte[] dn = new byte[len];
        for (Integer i = 0; i < len; i++) {
            dn[len - i - 1] = bytes[pos + i];
        }

        pos = pos + len;
        return new BigInteger(sign, dn);
    }

    private String parse_string(Integer len) {
        String s = new String(bytes, pos, len, StandardCharsets.UTF_8);
        pos = pos + len;
        return s;
    }

    private String parse_latin_1_string(Integer len) {
        String s = new String(bytes, pos, len, StandardCharsets.ISO_8859_1);
        pos = pos + len;
        return s;
    }

    @Override
    public String toString() {
        return "Request{" +
                "rawTerm=" + rawTerm +
                ", type=" + type +
                ", requestId=" + requestId +
                ", classname=" + classname +
                ", methodName=" + methodName +
                ", args=" + Arrays.toString(args) +
                ", message=" + message +
                ", context=" + context +
                '}';
    }
}
