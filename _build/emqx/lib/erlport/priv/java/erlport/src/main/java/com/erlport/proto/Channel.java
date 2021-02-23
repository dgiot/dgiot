package com.erlport.proto;

import com.erlport.core.ProtocolProcessor;
import com.erlport.erlang.term.Atom;
import com.erlport.erlang.term.Tuple;
import com.erlport.msg.CallMessage;
import com.erlport.msg.Message;
import com.erlport.msg.ResultMessage;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.ByteBuffer;


public class Channel {

    private Options opts;
    private InputStream in;
    private PrintStream out;

    public Channel(Options opts) {
        this.opts = opts;

        if (opts.method.equals("use_stdio")) {
            this.in = System.in;
            this.out = System.out;
        }
    }

    public int write(Response resp) throws Exception {
        byte[] body = resp.pack();

        // Compressing it
        if (opts.compressed > 0) {
            // TODO:
        }

        ByteBuffer bb = ByteBuffer.allocate(opts.packet + 1 + body.length);
        bb.put(encode_packet_length(body.length + 1, opts.packet));
        bb.put((byte) 131);
        bb.put(body);
        return writeData(bb.array());
    }

    /**
     * Write call message
     * {'C', Id, Module, Function, Args}
     * {'M', Terms}
     *
     * @param message
     */
    public void write(Message message) throws Exception {
        Tuple tuple;
        if (message instanceof CallMessage) {
            CallMessage callMessage = (CallMessage) message;
            tuple = new Tuple(6);
            tuple.set(0, callMessage.getType());
            tuple.set(1, message.getId());
            tuple.set(2, callMessage.getModule());
            tuple.set(3, callMessage.getFunction());
            tuple.set(4, callMessage.getArgs());
            tuple.set(5, new Atom("L"));
            writeData(serialize(tuple));
        }
        if (message instanceof ResultMessage) {
            ResultMessage resultMessage = new ResultMessage();
            tuple = new Tuple(3);
            tuple.set(0, resultMessage.getType());
            tuple.set(1, resultMessage.getId());
            tuple.set(2, resultMessage.getResult());
            writeData(serialize(tuple));
        }
    }

    /**
     * Serialize a tuple
     *
     * @param tuple
     * @return
     * @throws Exception
     */
    private byte[] serialize(Tuple tuple) throws Exception {
        byte[] bytes = ProtocolProcessor.serialize(tuple);
        ByteBuffer bb = ByteBuffer.allocate(opts.packet + 1 + bytes.length);
        bb.put(encode_packet_length(bytes.length + 1, opts.packet));
        bb.put((byte) 131);
        bb.put(bytes);
        return bb.array();
    }

    //--------------------------------------------------------------------------------------

    public Request read() throws Exception {
        int len = decode_packet_length(__read_data(opts.packet));

        // eof
        if (len == 0) {
            return null;
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
            int got = in.read(b, done, n - done);
            if (got == -1) {
                throw new EOFException("Read end of stream");
            }
            done = done + got;
        }
        return b;
    }

    private int writeData(byte[] bytes) {
        int len = bytes.length;
        out.write(bytes, 0, len);
        out.flush();
        return len;
    }

    // ------ Encode/Decode

    private int decode_packet_length(byte[] bytes) {

        if (bytes.length == 0)
            return 0;

        int m = 0x01 << ((bytes.length - 1) * 8);
        int c = 0;
        for (byte i : bytes) {
            c += (i & 0xff) * m;
            m = m >> 8;
        }
        return c;
    }

    private byte[] encode_packet_length(int len, int n) {

        ByteBuffer bb = ByteBuffer.allocate(n);
        for (int i = 0; i < n; i++) {
            bb.put((byte) (len >> (8 * (n - i - 1)) & 0xff));
        }
        return bb.array();
    }
}
