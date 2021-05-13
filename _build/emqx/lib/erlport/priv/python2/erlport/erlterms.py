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

"""Erlang external term format.

See Erlang External Term Format for details:
    http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
"""

__author__ = "Dmitry Vasiliev <dima@hlabs.org>"

from struct import Struct
from array import array
from zlib import decompressobj, compress
from cPickle import loads, dumps


# It seems protocol version 2 is supported by all Python versions
# from 2.5 to 3.2
PICKLE_PROTOCOL = 2

class IncompleteData(ValueError):
    """Need more data."""

    def __init__(self, data):
        self.data = data
        ValueError.__init__(self, "incomplete data: %r" % (data,))


class Atom(str):
    """Erlang atom."""

    __slots__ = ()

    __atoms = {}

    def __new__(cls, s):
        t = type(s)
        if t is Atom:
            return s
        elif not issubclass(t, str):
            raise TypeError("str object expected")
        elif len(s) > 255:
            raise ValueError("invalid atom length")
        s = intern(s)
        atoms = cls.__atoms
        if s not in atoms:
            atoms[s] = super(Atom, cls).__new__(cls, s)
        return atoms[s]

    def __repr__(self):
        return "Atom(%s)" % super(Atom, self).__repr__()


class List(list):
    """List which also can be converted to Unicode string."""

    __slots__ = ()

    def to_string(self):
        # Will raise TypeError if can't be converted
        return u"".join(map(unichr, self))

    def __repr__(self):
        return "List(%s)" % super(List, self).__repr__()


class ImproperList(list):
    """Improper list."""

    __slots__ = "tail"

    def __init__(self, lst, tail):
        if not isinstance(lst, list):
            raise TypeError("list object expected")
        elif not lst:
            raise ValueError("empty list not allowed")
        if isinstance(tail, list):
            raise TypeError("non list object expected for tail")
        self.tail = tail
        return super(ImproperList, self).__init__(lst)

    def __repr__(self):
        return "ImproperList(%s, %r)" % (
            super(ImproperList, self).__repr__(), self.tail)

    def __eq__(self, other):
        return (type(self) == type(other) and list(self) == list(other)
            and self.tail == other.tail)

    def __ne__(self, other):
        return not self == other


class OpaqueObject(object):
    """Opaque object data."""

    __slots__ = "data", "language"

    marker = Atom("$erlport.opaque")

    def __init__(self, data, language):
        if type(data) is not str:
            raise TypeError("data must be instance of str")
        if type(language) is not Atom:
            raise TypeError("language must be instance of Atom")
        self.data = data
        self.language = language

    @classmethod
    def decode(cls, data, language):
        if language == "python":
            return loads(data)
        return cls(data, language)

    def encode(self):
        if self.language == "erlang":
            return self.data
        return encode_term((self.marker, self.language, self.data))

    def __eq__(self, other):
        return (type(self) == type(other) and self.language == other.language
            and self.data == other.data)

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash((self.__class__, self.language, self.data))

    def __repr__(self):
        return "OpaqueObject(%r, %r)" % (self.data, self.language)


_python = Atom("python")

_int4_unpack = Struct(">I").unpack
_int2_unpack = Struct(">H").unpack
_signed_int4_unpack = Struct(">i").unpack
_float_unpack = Struct(">d").unpack
_double_bytes_unpack = Struct("BB").unpack
_int4_byte_unpack = Struct(">IB").unpack


def decode(string):
    """Decode Erlang external term."""
    if not string:
        raise IncompleteData(string)
    if string[0] != '\x83':
        raise ValueError("unknown protocol version: %r" % string[0])
    if string[1:2] == 'P':
        # compressed term
        if len(string) < 16:
            raise IncompleteData(string)
        d = decompressobj()
        term_string = d.decompress(string[6:]) + d.flush()
        uncompressed_size, = _int4_unpack(string[2:6])
        if len(term_string) != uncompressed_size:
            raise ValueError(
                "invalid compressed tag, "
                "%d bytes but got %d" % (uncompressed_size, len(term_string)))
        # tail data returned by decode_term() can be simple ignored
        term, _tail = decode_term(term_string)
        return term, d.unused_data
    return decode_term(string[1:])


def decode_term(string,
        # Hack to turn globals into locals
        len=len, ord=ord, tuple=tuple, float=float, array=array,
        int4_unpack=_int4_unpack, int2_unpack=_int2_unpack,
        signed_int4_unpack=_signed_int4_unpack, float_unpack=_float_unpack,
        double_bytes_unpack=_double_bytes_unpack,
        int4_byte_unpack=_int4_byte_unpack, Atom=Atom,
        opaque=OpaqueObject.marker, decode_opaque=OpaqueObject.decode):
    if not string:
        raise IncompleteData(string)
    tag = string[0]
    if tag == "d" or tag == "w":
        # ATOM_EXT, SMALL_ATOM_UTF8_EXT
        ln = len(string)
        if tag == "d":
            if ln < 3:
                raise IncompleteData(string)
            length = int2_unpack(string[1:3])[0] + 3
            if ln < length:
                raise IncompleteData(string)
            name = string[3:length]
        else:
            if ln < 2:
                raise IncompleteData(string)
            length = ord(string[1]) + 2
            if ln < length:
                raise IncompleteData(string)
            name = string[2:length]
        if name == "true":
            return True, string[length:]
        elif name == "false":
            return False, string[length:]
        elif name == "undefined":
            return None, string[length:]
        return Atom(name), string[length:]
    elif tag == "j":
        # NIL_EXT
        return List(), string[1:]
    elif tag == "k":
        # STRING_EXT
        ln = len(string)
        if ln < 3:
            raise IncompleteData(string)
        length = int2_unpack(string[1:3])[0] + 3
        if ln < length:
            raise IncompleteData(string)
        return List(array("B", string[3:length]).tolist()), string[length:]
    elif tag in "lhi":
        # LIST_EXT, SMALL_TUPLE_EXT, LARGE_TUPLE_EXT
        if tag == "h":
            if len(string) < 2:
                raise IncompleteData(string)
            length = ord(string[1])
            tail = string[2:]
        else:
            if len(string) < 5:
                raise IncompleteData(string)
            length, = int4_unpack(string[1:5])
            tail = string[5:]
        lst = List()
        append = lst.append
        _decode_term = decode_term
        while length > 0:
            term, tail = _decode_term(tail)
            append(term)
            length -= 1
        if tag == "l":
            if not tail:
                raise IncompleteData(string)
            if tail[0] != "j":
                improper_tail, tail = _decode_term(tail)
                return ImproperList(lst, improper_tail), tail
            return lst, tail[1:]
        if len(lst) == 3 and lst[0] == opaque:
            return decode_opaque(lst[2], lst[1]), tail
        return tuple(lst), tail
    elif tag == "a":
        # SMALL_INTEGER_EXT
        if len(string) < 2:
            raise IncompleteData(string)
        return ord(string[1]), string[2:]
    elif tag == "b":
        # INTEGER_EXT
        if len(string) < 5:
            raise IncompleteData(string)
        i, = signed_int4_unpack(string[1:5])
        return i, string[5:]
    elif tag == "m":
        # BINARY_EXT
        ln = len(string)
        if ln < 5:
            raise IncompleteData(string)
        length = int4_unpack(string[1:5])[0] + 5
        if ln < length:
            raise IncompleteData(string)
        return string[5:length], string[length:]
    elif tag == "F":
        # NEW_FLOAT_EXT
        if len(string) < 9:
            raise IncompleteData(string)
        f, = float_unpack(string[1:9])
        return f, string[9:]
    elif tag in "no":
        # SMALL_BIG_EXT, LARGE_BIG_EXT
        if tag == "n":
            if len(string) < 3:
                raise IncompleteData(string)
            length, sign = double_bytes_unpack(string[1:3])
            tail = string[3:]
        else:
            if len(string) < 6:
                raise IncompleteData(string)
            length, sign = int4_byte_unpack(string[1:6])
            tail = string[6:]
        if len(tail) < length:
            raise IncompleteData(string)
        n = 0
        if length:
            for i in array("B", tail[length-1::-1]):
                n = (n << 8) | i
            if sign:
                n = -n
        return n, tail[length:]

    raise ValueError("unsupported data: %r" % (string,))

_int4_pack = Struct(">I").pack
_char_int4_pack = Struct(">cI").pack
_char_int2_pack = Struct(">cH").pack
_char_signed_int4_pack = Struct(">ci").pack
_char_float_pack = Struct(">cd").pack
_char_2bytes_pack = Struct("cBB").pack
_char_int4_byte_pack = Struct(">cIB").pack

def encode(term, compressed=False):
    """Encode Erlang external term."""
    encoded_term = encode_term(term)
    # False and 0 do not attempt compression.
    if compressed:
        if compressed is True:
            # default compression level of 6
            compressed = 6
        elif compressed < 0 or compressed > 9:
            raise ValueError("invalid compression level: %r" % (compressed,))
        zlib_term = compress(encoded_term, compressed)
        ln = len(encoded_term)
        if len(zlib_term) + 5 <= ln:
            # Compressed term should be smaller
            return '\x83P' + _int4_pack(ln) + zlib_term
    return "\x83" + encoded_term


def encode_term(term,
        # Hack to turn globals into locals
        tuple=tuple, len=len, list=list, int=int, long=long, type=type,
        array=array, unicode=unicode, Atom=Atom, str=str, map=map, float=float,
        ord=ord, dict=dict, True=True, False=False, dumps=dumps,
        PICKLE_PROTOCOL=PICKLE_PROTOCOL, OpaqueObject=OpaqueObject,
        ImproperList=ImproperList, char_int4_pack=_char_int4_pack,
        char_int2_pack=_char_int2_pack,
        char_signed_int4_pack=_char_signed_int4_pack, List=List,
        char_float_pack=_char_float_pack, char_2bytes_pack=_char_2bytes_pack,
        char_int4_byte_pack=_char_int4_byte_pack, python=_python):
    t = type(term)
    if t is tuple:
        arity = len(term)
        if arity <= 255:
            header = "h%c" % arity
        elif arity <= 4294967295:
            header = char_int4_pack('i', arity)
        else:
            raise ValueError("invalid tuple arity: %r" % arity)
        return header + "".join(map(encode_term, term))
    elif t is list or t is List:
        length = len(term)
        if not term:
            return "j"
        elif length <= 65535:
            try:
                # Array coersion will allow floats as a deprecated feature in
                # Python 2.6 and previous versions
                for t in term:
                    typ = type(t)
                    if not (typ is int or typ is long):
                        raise TypeError
                b = array('B', term).tostring()
            except (TypeError, OverflowError):
                pass
            else:
                return char_int2_pack('k', length) + b
        elif length > 4294967295:
            raise ValueError("invalid list length: %r" % length)
        return (char_int4_pack('l', length)
            + "".join(map(encode_term, term)) + "j")
    elif t is unicode:
        return encode_term(map(ord, term))
    elif t is Atom:
        return char_int2_pack('d', len(term)) + term
    elif t is str:
        length = len(term)
        if length > 4294967295:
            raise ValueError("invalid binary length: %r" % length)
        return char_int4_pack('m', length) + term
    elif term is True:
        return "d\0\4true"
    elif term is False:
        return "d\0\5false"
    elif t is int or t is long:
        if 0 <= term <= 255:
            return 'a%c' % term
        elif -2147483648 <= term <= 2147483647:
            return char_signed_int4_pack('b', term)

        if term >= 0:
            sign = 0
        else:
            sign = 1
            term = -term

        bytes = array('B')
        append = bytes.append
        while term:
            append(term & 0xff)
            term >>= 8

        length = len(bytes)
        if length <= 255:
            return char_2bytes_pack('n', length, sign) + bytes.tostring()
        elif length <= 4294967295:
            return char_int4_byte_pack('o', length, sign) + bytes.tostring()
        raise ValueError("invalid integer value with length: %r" % length)
    elif t is float:
        return char_float_pack('F', term)
    elif term is None:
        return "d\0\11undefined"
    elif t is OpaqueObject:
        return term.encode()
    elif t is ImproperList:
        length = len(term)
        if length > 4294967295:
            raise ValueError("invalid improper list length: %r" % length)
        header = char_int4_pack('l', length)
        return header + "".join(map(encode_term, term)) + encode_term(term.tail)

    try:
        data = dumps(term, PICKLE_PROTOCOL)
    except:
        raise ValueError("unsupported data type: %s" % type(term))
    return OpaqueObject(data, python).encode()
