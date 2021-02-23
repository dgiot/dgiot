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

import os
import errno
import unittest

from erlport.erlproto import Port
from erlport.erlterms import Atom


class TestPortClient(object):

    def __init__(self, **kwargs):
        r, self.out_d = os.pipe()
        self.in_d, w = os.pipe()
        self.port = Port(descriptors=(r, w), **kwargs)

    def read(self):
        return os.read(self.in_d, 65536)

    def write(self, data):
        return os.write(self.out_d, data)

    def close(self):
        os.close(self.in_d)
        os.close(self.out_d)

class PortTestCase(unittest.TestCase):

    def test_default_port_read(self):
        client = TestPortClient()
        self.assertEqual(12, client.write(b"\0\0\0\10\x83d\0\4test"))
        atom = client.port.read()
        self.assertTrue(isinstance(atom, Atom))
        self.assertEqual(Atom(b"test"), atom)

    def test_default_port_write(self):
        client = TestPortClient()
        self.assertEqual(12, client.port.write(Atom(b"test")))
        self.assertEqual(b"\0\0\0\10\x83d\0\4test", client.read())

    def test_invalid_packet_value(self):
        self.assertRaises(ValueError, Port, packet=0)
        self.assertRaises(ValueError, Port, packet=3)

    def test_use_stdio(self):
        port = Port()
        self.assertEqual(0, port.in_d)
        self.assertEqual(1, port.out_d)
        port = Port(use_stdio=True)
        self.assertEqual(0, port.in_d)
        self.assertEqual(1, port.out_d)

    def test_nouse_stdio(self):
        port = Port(use_stdio=False)
        self.assertEqual(3, port.in_d)
        self.assertEqual(4, port.out_d)

    def test_descriptors(self):
        port = Port(descriptors=(10, 20))
        self.assertEqual(10, port.in_d)
        self.assertEqual(20, port.out_d)

    def test_port_close(self):
        client = TestPortClient()
        client.port.close()
        self.assertRaises(OSError, client.write, b"data")
        self.assertEqual(b"", client.read())

    def test_closed_port(self):
        client = TestPortClient()
        client.close()
        self.assertRaises(EOFError, client.port.read)
        self.assertRaises(EOFError, client.port.write, b"data")

    def test_read_multiple_terms(self):
        client = TestPortClient()
        atom_data = b"\0\0\0\10\x83d\0\4test"
        self.assertEqual(24, client.write(atom_data + atom_data))
        atom = client.port.read()
        self.assertTrue(isinstance(atom, Atom))
        self.assertEqual(Atom(b"test"), atom)
        atom = client.port.read()
        self.assertTrue(isinstance(atom, Atom))
        self.assertEqual(Atom(b"test"), atom)

    def test_small_buffer_read(self):
        client = TestPortClient(buffer_size=1)
        self.assertEqual(12, client.write(b"\0\0\0\10\x83d\0\4test"))
        atom = client.port.read()
        self.assertTrue(isinstance(atom, Atom))
        self.assertEqual(Atom(b"test"), atom)

    def test_invalid_buffer_size(self):
        self.assertRaises(ValueError, Port, buffer_size=0)

    def test_packet4_port_read(self):
        client = TestPortClient(packet=4)
        self.assertEqual(12, client.write(b"\0\0\0\10\x83d\0\4test"))
        atom = client.port.read()
        self.assertTrue(isinstance(atom, Atom))
        self.assertEqual(Atom(b"test"), atom)

    def test_packet4_port_write(self):
        client = TestPortClient(packet=4)
        self.assertEqual(12, client.port.write(Atom(b"test")))
        self.assertEqual(b"\0\0\0\10\x83d\0\4test", client.read())

    def test_packet2_port_read(self):
        client = TestPortClient(packet=2)
        self.assertEqual(10, client.write(b"\0\10\x83d\0\4test"))
        atom = client.port.read()
        self.assertTrue(isinstance(atom, Atom))
        self.assertEqual(Atom(b"test"), atom)

    def test_packet2_port_write(self):
        client = TestPortClient(packet=2)
        self.assertEqual(10, client.port.write(Atom(b"test")))
        self.assertEqual(b"\0\10\x83d\0\4test", client.read())

    def test_packet1_port_read(self):
        client = TestPortClient(packet=1)
        self.assertEqual(9, client.write(b"\10\x83d\0\4test"))
        atom = client.port.read()
        self.assertTrue(isinstance(atom, Atom))
        self.assertEqual(Atom(b"test"), atom)

    def test_packet1_port_write(self):
        client = TestPortClient(packet=1)
        self.assertEqual(9, client.port.write(Atom(b"test")))
        self.assertEqual(b"\10\x83d\0\4test", client.read())

    def test_compressed_port_read(self):
        client = TestPortClient(packet=1, compressed=True)
        self.assertEqual(26, client.write(b"\x19\x83P\0\0\0\x1a\x78\x9c\xcb\x61"
            b"\x60\x60\x60\xcd\x66\x60\xd4\x43\xc7\x59\0\x30\x48\3\xde"))
        self.assertEqual([[46], [46], [46], [46], [46]], client.port.read())

    def test_compressed_port_write(self):
        client = TestPortClient(packet=1, compressed=True)
        self.assertEqual(26, client.port.write([[46], [46], [46], [46], [46]]))
        self.assertEqual(b"\x19\x83P\0\0\0\x1a\x78\x9c\xcb\x61"
            b"\x60\x60\x60\xcd\x66\x60\xd4\x43\xc7\x59\0\x30\x48\3\xde",
            client.read())

    def test_slow_write(self):
        write = os.write
        os.write = lambda d, data: 1
        try:
            port = Port(packet=1)
            self.assertEqual(9, port.write(Atom(b"test")))
        finally:
            os.write = write

    def test_no_data_written(self):
        write = os.write
        os.write = lambda d, data: 0
        try:
            port = Port()
            self.assertRaises(EOFError, port.write, b"test")
        finally:
            os.write = write

    def test_error_on_write(self):
        def test_write(d, data):
            raise OSError()
        write = os.write
        os.write = test_write
        try:
            port = Port()
            self.assertRaises(OSError, port.write, b"test")
        finally:
            os.write = write

    def test_error_on_read(self):
        def test_read(d, buffer_size):
            raise OSError()
        read = os.read
        os.read = test_read
        try:
            port = Port()
            self.assertRaises(OSError, port.read)
        finally:
            os.read = read

    def test_close_on_read(self):
        def test_read(d, buffer_size):
            raise OSError(errno.EPIPE, "Pipe closed")
        read = os.read
        os.read = test_read
        try:
            port = Port()
            self.assertRaises(EOFError, port.read)
        finally:
            os.read = read


def get_suite():
    load = unittest.TestLoader().loadTestsFromTestCase
    suite = unittest.TestSuite()
    suite.addTests(load(PortTestCase))
    return suite
