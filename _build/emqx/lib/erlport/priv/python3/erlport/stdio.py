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

import sys
import io

from erlport import Atom


class RedirectedStdin(io.TextIOBase):

    def __init__(self):
        self.close()

    def readable(self):
        return True

    def isatty(self):
        return True

    def fileno(self):
        return 0

    def seekable(self):
        return False

    def writable(self):
        return False

    @property
    def encoding(self):
        return "UTF-8"

    @property
    def mode(self):
        return "r"

    @property
    def name(self):
        return "<stdin>"

class RedirectedStdout(io.TextIOBase):

    def __init__(self, port):
        self.__port = port

    def readable(self):
        return False

    def isatty(self):
        return True

    def fileno(self):
        return 1

    def seekable(self):
        return False

    def writable(self):
        return True

    def write(self, data):
        if self.closed:
            raise ValueError("I/O operation on closed file")
        if not isinstance(data, str):
            raise TypeError("must be str, not %s" % data.__class__.__name__)
        return self.__port.write((Atom(b"P"), data))

    def writelines(self, lst):
        if self.closed:
            raise ValueError("I/O operation on closed file")
        for data in lst:
            if not isinstance(data, str):
                raise TypeError("must be str, not %s" % data.__class__.__name__)
        return self.write("".join(lst))

    @property
    def encoding(self):
        return "UTF-8"

    @property
    def mode(self):
        return "w"

    @property
    def name(self):
        return "<stdout>"


def redirect(port):
    sys.stdin = RedirectedStdin()
    sys.stdout = RedirectedStdout(port)
