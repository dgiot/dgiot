# encoding: binary
# Copyright (c) 2009-2015, Dmitry Vasiliev <dima@hlabs.org>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#  * Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#  * Neither the name of the copyright holders nor the names of its
#    contributors may be used to endorse or promote products derived from this
#    software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

require 'test/unit'
require 'erlport/erlproto'
require 'erlport/erlterms'

include ErlPort::ErlProto
include ErlPort::ErlTerm

class TestPortClient
    attr_reader :port

    def initialize packet=4, use_stdio=true, compressed=false, \
            buffer_size=65536
        r, @out = IO.pipe
        @in, w = IO.pipe
        @port = Port.new(packet, use_stdio, compressed, \
            descriptors=[r.fileno, w.fileno], buffer_size)
    end

    def read
        @in.sysread(65536)
    end

    def write data
        @out.syswrite(data)
    end

    def close
        @in.close
        @out.close
    end
end

class PortTestCase < Test::Unit::TestCase
    def test_default_port_read
        client = TestPortClient.new
        assert_equal 12, client.write("\0\0\0\10\x83d\0\4test")
        atom = client.port.read
        assert atom.is_a? Symbol
        assert_equal :test, atom
    end

    def test_default_port_write
        client = TestPortClient.new
        assert_equal 12, client.port.write(:test)
        assert_equal "\0\0\0\10\x83d\0\4test", client.read
    end

    def test_invalid_packet_value
        assert_raise(ValueError){Port.new(0)}
        assert_raise(ValueError){Port.new(3)}
    end

    def test_use_stdio
        port = Port.new()
        assert_equal 0, port.in_d
        assert_equal 1, port.out_d
        port = Port.new(4, true)
        assert_equal 0, port.in_d
        assert_equal 1, port.out_d
    end

    def test_port_close
        client = TestPortClient.new
        client.port.close
        assert_raise(Errno::EPIPE, Errno::EINVAL){client.write("data")}
        assert_raise(EOFError){client.read}
    end

    def test_closed_port
        client = TestPortClient.new
        # client.port.read can raise Errno::EBADF sometimes
        assert_raise(EOFError){client.close; client.port.read}
        assert_raise(EOFError){client.port.write("data")}
    end

    def test_read_multiple_terms
        client = TestPortClient.new
        atom_data = "\0\0\0\10\x83d\0\4test"
        assert_equal 24, client.write(atom_data + atom_data)
        atom = client.port.read
        assert atom.is_a? Symbol
        assert_equal :test, atom
        atom = client.port.read()
        assert atom.is_a? Symbol
        assert_equal :test, atom
    end

    def test_small_buffer_read
        client = TestPortClient.new(4, true, false, buffer_size=1)
        assert_equal 12, client.write("\0\0\0\10\x83d\0\4test")
        atom = client.port.read
        assert atom.is_a? Symbol
        assert_equal :test, atom
    end

    def test_invalid_buffer_size
        assert_raise(ValueError){Port.new(4, true, false, nil, buffer_size=0)}
    end

    def test_packet4_port_read
        client = TestPortClient.new(packet=4)
        assert_equal 12, client.write("\0\0\0\10\x83d\0\4test")
        atom = client.port.read
        assert atom.is_a? Symbol
        assert_equal :test, atom
    end

    def test_packet4_port_write
        client = TestPortClient.new(packet=4)
        assert_equal 12, client.port.write(:test)
        assert_equal "\0\0\0\10\x83d\0\4test", client.read
    end

    def test_packet2_port_read
        client = TestPortClient.new(packet=2)
        assert_equal 10, client.write("\0\10\x83d\0\4test")
        atom = client.port.read
        assert atom.is_a? Symbol
        assert_equal :test, atom
    end

    def test_packet2_port_write
        client = TestPortClient.new(packet=2)
        assert_equal 10, client.port.write(:test)
        assert_equal "\0\10\x83d\0\4test", client.read
    end

    def test_packet1_port_read
        client = TestPortClient.new(packet=1)
        assert_equal 9, client.write("\10\x83d\0\4test")
        atom = client.port.read
        assert atom.is_a? Symbol
        assert_equal :test, atom
    end

    def test_packet1_port_write
        client = TestPortClient.new(packet=1)
        assert_equal 9, client.port.write(:test)
        assert_equal "\10\x83d\0\4test", client.read
    end

    def test_compressed_port_read
        client = TestPortClient.new(packet=1, true, compressed=true)
        assert_equal 26, client.write("\x19\x83P\0\0\0\x1a\x78\x9c\xcb\x61" \
            "\x60\x60\x60\xcd\x66\x60\xd4\x43\xc7\x59\0\x30\x48\3\xde")
        assert_equal [[46], [46], [46], [46], [46]], client.port.read
    end

    def test_compressed_port_write
        client = TestPortClient.new(packet=1, true, compressed=true)
        assert_equal 26, client.port.write([[46], [46], [46], [46], [46]])
        assert_equal "\x19\x83P\0\0\0\x1a\x78\x9c\xcb\x61" \
            "\x60\x60\x60\xcd\x66\x60\xd4\x43\xc7\x59\0\x30\x48\3\xde",
            client.read
    end
end
