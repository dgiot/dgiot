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
import errno

from erlport import Atom


class RedirectedStdin(object):

    def close(self):
        pass

    def flush(self):
        pass

    def fileno(self):
        return 0

    def isatty(self):
        return True

    def next(self):
        raise ValueError("I/O operation on closed file")

    def __iter__(self):
        raise ValueError("I/O operation on closed file")

    def read(self, size=None):
        raise ValueError("I/O operation on closed file")

    def readline(self, size=None):
        raise ValueError("I/O operation on closed file")

    def readlines(self, sizehint=None):
        raise ValueError("I/O operation on closed file")

    def xreadlines(self):
        raise ValueError("I/O operation on closed file")

    def seek(self, offset, whence=None):
        raise IOError(errno.ESPIPE, "Illegal seek")

    def tell(self):
        raise IOError(errno.ESPIPE, "Illegal seek")

    def truncate(self, size=None):
        raise IOError("File not open for writing")

    def write(self, data):
        raise IOError("File not open for writing")

    def writelines(self, lst):
        raise IOError("File not open for writing")

    @property
    def closed(self):
        return True

    @property
    def encoding(self):
        return "UTF-8"

    @property
    def errors(self):
        return None

    @property
    def mode(self):
        return "r"

    @property
    def name(self):
        return "<stdin>"

    @property
    def newlines(self):
        return None

    @property
    def softspace(self):
        return False


class RedirectedStdout(object):

    def __init__(self, port):
        self.__port = port
        self.__closed = False

    def close(self):
        self.__closed = True

    def flush(self):
        pass

    def fileno(self):
        return 1

    def isatty(self):
        return True

    def next(self):
        raise IOError("File not open for reading")

    def __iter__(self):
        raise IOError("File not open for reading")

    def read(self, size=None):
        raise IOError("File not open for reading")

    def readline(self, size=None):
        raise IOError("File not open for reading")

    def readlines(self, sizehint=None):
        raise IOError("File not open for reading")

    def xreadlines(self):
        raise IOError("File not open for reading")

    def seek(self, offset, whence=None):
        raise IOError(errno.ESPIPE, "Illegal seek")

    def tell(self):
        raise IOError(errno.ESPIPE, "Illegal seek")

    def truncate(self, size=None):
        raise IOError(errno.ESPIPE, "Illegal seek")

    def write(self, data):
        if self.__closed:
            raise ValueError("I/O operation on closed file")
        if not isinstance(data, (str, unicode, buffer)):
            raise TypeError("expected a characer buffer object")
        return self.__port.write((Atom("P"), data))

    def writelines(self, lst):
        if self.__closed:
            raise ValueError("I/O operation on closed file")
        for data in lst:
            if not isinstance(data, (str, unicode, buffer)):
                raise TypeError("expected a character buffer object")
        return self.write("".join(lst))

    @property
    def closed(self):
        return self.__closed

    @property
    def encoding(self):
        return "UTF-8"

    @property
    def errors(self):
        return None

    @property
    def mode(self):
        return "w"

    @property
    def name(self):
        return "<stdout>"

    @property
    def newlines(self):
        return None

    @property
    def softspace(self):
        return False


def redirect(port):
    sys.stdin = RedirectedStdin()
    sys.stdout = RedirectedStdout(port)
