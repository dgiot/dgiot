package erlport;

import java.io.*;
import java.nio.*;
import java.lang.*;
import java.util.*;

import erlport.terms.*;

public class Port extends Object {

    Options opts;

    InputStream in;
    PrintStream out;

    public Port(Options opts) {
        this.opts = opts;

        if (opts.method== "use_stdio") {
            this.in = System.in;
            this.out = System.out;
        } else {
            // TODO:
        }
    }

    public int write(Response resp) throws Exception {
        byte[] body = resp.pack();

        // Compressing it
        if (opts.compressed > 0) {
            // TODO:
        }

        ByteBuffer bb = ByteBuffer.allocate(opts.packet + 1 + body.length);
        bb.put( encode_packet_length(body.length + 1, opts.packet) );
        bb.put( (byte) 131 );
        bb.put( body );

        return __write__data(bb.array());
    }

    public Request read() throws Exception {
        int len = decode_packet_length(__read_data(opts.packet));

        // eof
        if (len == 0 ) {
            throw new EOFException("end of stream");
        }

        return new Request(__read_data(len));
    }

    private byte[] __read_data(int n) throws IOException {

        if (n == 0) {
            return new byte[]{};
        }

        byte[] b = new byte[n];

        int done = 0;
        while (done < n) {
            int got = in.read(b, done, n-done);
            if (got == -1) {
                throw new EOFException("end of stream");
            }
            done = done + got;
        }

        //System.err.printf("Got: %d, bytes: %s\n", b.length, Arrays.toString(b));
        return b;
    }

    public int __write__data(byte [] bytes) {
        int len = bytes.length;
        out.write(bytes, 0, len);
        out.flush();

        //System.err.printf("Write: %s\n", Arrays.toString(bytes));
        return len;
    }

    // ------ Encode/Decode

    private int decode_packet_length(byte[] bytes) {

        if (bytes.length == 0)
            return 0;

        int m = 0x01 << ((bytes.length - 1)*8);
        int c = 0;
        for (byte i: bytes) {
            c += (i & 0xff) * m;
            m = m >> 8;
        }
        return c;
    }

    private byte[] encode_packet_length(int len, int n) {

        ByteBuffer bb = ByteBuffer.allocate(n);
        for(Integer i=0; i<n; i++) {
            bb.put( (byte) (len >> (8*(n-i-1)) & 0xff) );
        }
        return bb.array();
    }
}
