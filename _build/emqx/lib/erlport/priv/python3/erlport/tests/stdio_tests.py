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

import unittest

from erlport import Atom
from erlport.stdio import RedirectedStdin, RedirectedStdout


class TestPort(object):

    def write(self, data):
        return data

class RedirectedStdinTestCase(unittest.TestCase):

    def test_methods(self):
        stdin = RedirectedStdin()
        self.assertEqual(0, stdin.fileno())
        self.assertEqual(True, stdin.isatty())
        self.assertEqual(None, stdin.close())
        self.assertRaises(ValueError, stdin.flush)
        self.assertRaises(ValueError, stdin.read)
        self.assertRaises(ValueError, stdin.readline)
        self.assertRaises(ValueError, stdin.readlines)
        self.assertRaises(IOError, stdin.seek, 0)
        self.assertRaises(IOError, stdin.tell)
        self.assertRaises(IOError, stdin.truncate)
        self.assertRaises(ValueError, stdin.write, "data")
        self.assertRaises(ValueError, stdin.writelines, ["da", "ta"])

    def test_attributes(self):
        stdin = RedirectedStdin()
        self.assertEqual(True, stdin.closed)
        self.assertEqual("UTF-8", stdin.encoding)
        self.assertEqual(None, stdin.errors)
        self.assertEqual("r", stdin.mode)
        self.assertEqual("<stdin>", stdin.name)
        self.assertEqual(None, stdin.newlines)

class RedirectedStdoutTestCase(unittest.TestCase):

    def test_write(self):
        stdout = RedirectedStdout(TestPort())
        self.assertEqual((Atom(b"P"), "data"), stdout.write("data"))
        self.assertRaises(TypeError, stdout.write, 1234)

    def test_writelines(self):
        stdout = RedirectedStdout(TestPort())
        self.assertEqual((Atom(b"P"), "data"), stdout.writelines(["da", "ta"]))
        self.assertRaises(TypeError, stdout.writelines, ["da", 1234])

    def test_close(self):
        stdout = RedirectedStdout(TestPort())
        self.assertEqual(False, stdout.closed)
        self.assertEqual(None, stdout.close())
        self.assertEqual(True, stdout.closed)
        self.assertEqual(None, stdout.close())
        self.assertRaises(ValueError, stdout.write, "data")
        self.assertRaises(ValueError, stdout.writelines, ["da", "ta"])

    def test_methods(self):
        stdout = RedirectedStdout(TestPort())
        self.assertEqual(1, stdout.fileno())
        self.assertEqual(True, stdout.isatty())
        self.assertEqual(None, stdout.flush())
        self.assertEqual(False, stdout.readable())
        self.assertEqual(False, stdout.seekable())
        self.assertEqual(True, stdout.writable())
        self.assertRaises(IOError, stdout.read)
        self.assertRaises(IOError, stdout.readline)
        self.assertRaises(IOError, stdout.readlines)
        self.assertRaises(IOError, stdout.seek, 0)
        self.assertRaises(IOError, stdout.tell)
        self.assertRaises(IOError, stdout.truncate)

    def test_attributes(self):
        stdout = RedirectedStdout(TestPort())
        self.assertEqual(False, stdout.closed)
        self.assertEqual("UTF-8", stdout.encoding)
        self.assertEqual(None, stdout.errors)
        self.assertEqual("w", stdout.mode)
        self.assertEqual("<stdout>", stdout.name)
        self.assertEqual(None, stdout.newlines)


def get_suite():
    load = unittest.TestLoader().loadTestsFromTestCase
    suite = unittest.TestSuite()
    suite.addTests(load(RedirectedStdinTestCase))
    suite.addTests(load(RedirectedStdoutTestCase))
    return suite
