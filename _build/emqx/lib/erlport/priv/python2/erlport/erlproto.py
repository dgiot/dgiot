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

"""Erlang port protocol."""

from __future__ import with_statement

__author__ = "Dmitry Vasiliev <dima@hlabs.org>"

import os
import errno
from struct import Struct
from threading import Lock

from erlport.erlterms import encode, decode


class Port(object):
    """Erlang port."""

    _formats = {
        1: Struct("B"),
        2: Struct(">H"),
        4: Struct(">I"),
        }

    def __init__(self, packet=4, use_stdio=True, compressed=False,
            descriptors=None, buffer_size=65536):
        if buffer_size < 1:
            raise ValueError("invalid buffer size value: %s" % (buffer_size,))
        struct = self._formats.get(packet)
        if struct is None:
            raise ValueError("invalid packet size value: %s" % (packet,))
        self.__pack = struct.pack
        self.__unpack = struct.unpack
        self.packet = packet
        self.compressed = compressed

        if descriptors is not None:
            self.in_d, self.out_d = descriptors
        elif use_stdio:
            self.in_d, self.out_d = 0, 1
        else:
            self.in_d, self.out_d = 3, 4

        self.__buffer = ""
        self.buffer_size = buffer_size
        self.__read_lock = Lock()
        self.__write_lock = Lock()

    def _read_data(self):
        try:
            buf = os.read(self.in_d, self.buffer_size)
        except OSError, why:
            if why.errno in (errno.EPIPE, errno.EINVAL):
                raise EOFError()
            raise
        if not buf:
            raise EOFError()
        return buf

    def read(self):
        """Read incoming message."""
        packet = self.packet
        with self.__read_lock:
            buffer = self.__buffer
            while len(buffer) < packet:
                buffer += self._read_data()
            length = self.__unpack(buffer[:packet])[0] + packet
            while len(buffer) < length:
                buffer += self._read_data()
            term, self.__buffer = decode(buffer[packet:])
        return term

    def write(self, message):
        """Write outgoing message."""
        data = encode(message, compressed=self.compressed)
        length = len(data)
        data = self.__pack(length) + data
        with self.__write_lock:
            while data:
                try:
                    n = os.write(self.out_d, data)
                except OSError, why:
                    if why.errno in (errno.EPIPE, errno.EINVAL):
                        raise EOFError()
                    raise
                if not n:
                    raise EOFError()
                data = data[n:]
        return length + self.packet

    def close(self):
        """Close port."""
        os.close(self.in_d)
        os.close(self.out_d)
