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
require 'erlport/erlterms'

include ErlPort::ErlTerm


class EmptySymbolTestCase < Test::Unit::TestCase
    def test_empty_symbol
        e = EmptySymbol.new
        e2 = EmptySymbol.new
        assert_equal e, e2
        assert_equal "", e.to_s
        assert_equal e.to_s, e2.to_s
    end
end

class TupleTestCase < Test::Unit::TestCase
    def test_tuple
        tuple = Tuple.new([1, 2, 3])
        assert_equal Tuple, tuple.class
        assert_equal 3, tuple.length
        assert_equal 1, tuple[0]
        assert_equal 2, tuple[1]
        assert_equal 3, tuple[2]
        assert_equal [1, 2, 3], tuple.to_a
    end

    def test_multiple_assignment
        tuple = Tuple.new([1, 2, 3])
        a, b, c = tuple
        assert_equal a, 1
        assert_equal b, 2
        assert_equal c, 3
    end

    def test_comparison
        tuple = Tuple.new([1, 2, 3])
        assert_equal tuple, tuple
        assert_equal tuple, Tuple.new([1, 2, 3])
        assert_not_equal tuple, Tuple.new([3, 2, 1])
    end
end

class OpaqueObjectTestCase < Test::Unit::TestCase
    def test_opaque_object
        obj = OpaqueObject.new "data", :language
        assert_equal "data", obj.data
        assert_equal :language, obj.language
        assert_raise(TypeError){OpaqueObject.new("data", "language")}
        assert_raise(TypeError){OpaqueObject.new([1, 2], :language)}
    end

    def test_comparison
        obj = OpaqueObject.new "data", :language
        assert_equal obj, obj
        assert_equal obj, OpaqueObject.new("data", :language)
        assert_not_equal obj, OpaqueObject.new("data", :language2)
        assert_not_equal obj, OpaqueObject.new("data2", :language)
    end

    def test_decode
        obj = OpaqueObject.decode "data", :language
        assert_equal "data", obj.data
        assert_equal :language, obj.language
    end

    def test_decode_ruby
        data = OpaqueObject.decode "\004\b\"\ttest", :ruby
        assert_equal "test", data
    end

    def test_encode
        obj = OpaqueObject.new("data", :language)
        term = Tuple.new([:"$erlport.opaque", :language, "data"])
        assert_equal encode(term), "\x83" + obj.encode()
    end

    def test_encode_erlang
        obj = OpaqueObject.new "data", :erlang
        assert_equal "data", obj.encode
    end

    def test_hashing
        obj = OpaqueObject.new "data", :language
        obj2 = OpaqueObject.new "data", :language
        assert_equal obj.hash, obj2.hash
        obj3 = OpaqueObject.new "data", :language2
        assert_not_equal obj.hash, obj3.hash
        obj4 = OpaqueObject.new "data2", :language
        assert_not_equal obj.hash, obj4.hash
    end
end

class ImproperListTestCase < Test::Unit::TestCase
    def test_improper_list
        improper = ImproperList.new [1, 2, 3], "tail"
        assert_equal ImproperList, improper.class
        assert_equal [1, 2, 3], improper
        assert_equal "tail", improper.tail
    end

    def test_comparison
        improper = ImproperList.new [1, 2, 3], "tail"
        assert_equal improper, improper
        assert_equal improper, ImproperList.new([1, 2, 3], "tail")
        assert_not_equal improper, ImproperList.new([1, 2, 3], "tail2")
        assert_not_equal improper, ImproperList.new([1, 2], "tail")
    end

    def test_hashing
        hash = ImproperList.new([1, 2, 3], "tail").hash
        assert_equal hash, ImproperList.new([1, 2, 3], "tail").hash
        assert_not_equal hash, ImproperList.new([1, 2, 3], "tail2").hash
        assert_not_equal hash, ImproperList.new([1, 2], "tail").hash
    end

    def test_errors
        assert_raise(TypeError){ImproperList.new([1, 2, 3], ["invalid"])}
        assert_raise(ValueError){ImproperList.new([], "tail")}
    end
end

class DecodeTestCase < Test::Unit::TestCase
    def test_decode
        assert_raise(IncompleteData){decode("")}
        assert_raise(ValueError){decode("\0")}
        assert_raise(IncompleteData){decode("\x83")}
        assert_raise(ValueError){decode("\x83z")}
    end

    def test_decode_atom
        assert_raise(IncompleteData){decode("\x83d")}
        assert_raise(IncompleteData){decode("\x83d\0")}
        assert_raise(IncompleteData){decode("\x83d\0\1")}
        assert_equal EmptySymbol, decode("\x83d\0\0")[0].class
        assert_equal [EmptySymbol.new, ""], decode("\x83d\0\0")
        assert_equal [EmptySymbol.new, "tail"], decode("\x83d\0\0tail")
        assert_equal [:test, ""], decode("\x83d\0\4test")
        assert_equal [:test, "tail"], decode("\x83d\0\4testtail")
    end

    def test_decode_predefined_atoms
        assert_equal [true, ""], decode("\x83d\0\4true")
        assert_equal [true, "tail"], decode("\x83d\0\4truetail")
        assert_equal [false, ""], decode("\x83d\0\5false")
        assert_equal [false, "tail"], decode("\x83d\0\5falsetail")
        assert_equal [nil, ""], decode("\x83d\0\11undefined")
        assert_equal [nil, "tail"], decode("\x83d\0\11undefinedtail")
    end

    def test_decode_empty_list
        assert_equal [[], ""], decode("\x83j")
        assert_equal [[], "tail"], decode("\x83jtail")
    end

    def test_decode_string_list
        assert_raise(IncompleteData){decode("\x83k")}
        assert_raise(IncompleteData){decode("\x83k\0")}
        assert_raise(IncompleteData){decode("\x83k\0\1")}
        # Erlang use 'j' tag for empty lists
        assert_equal [[], ""], decode("\x83k\0\0")
        assert_equal [[], "tail"], decode("\x83k\0\0tail")
        assert_equal [[116, 101, 115, 116], ""], decode("\x83k\0\4test")
        assert_equal [[116, 101, 115, 116], "tail"],
            decode("\x83k\0\4testtail")
    end

    def test_decode_list
        assert_raise(IncompleteData){decode("\x83l")}
        assert_raise(IncompleteData){decode("\x83l\0")}
        assert_raise(IncompleteData){decode("\x83l\0\0")}
        assert_raise(IncompleteData){decode("\x83l\0\0\0")}
        assert_raise(IncompleteData){decode("\x83l\0\0\0\0")}
        # Erlang use 'j' tag for empty lists
        assert_equal [[], ""], decode("\x83l\0\0\0\0j")
        assert_equal [[], "tail"], decode("\x83l\0\0\0\0jtail")
        assert_equal [[[], []], ""], decode("\x83l\0\0\0\2jjj")
        assert_equal [[[], []], "tail"], decode("\x83l\0\0\0\2jjjtail")
    end

    def test_decode_improper_list
        assert_raise(IncompleteData){decode("\x83l\0\0\0\0k")}
        improper, tail = decode("\x83l\0\0\0\1jd\0\4tail")
        assert_equal ImproperList, improper.class
        assert_equal [[]], improper
        assert_equal Symbol, improper.tail.class
        assert_equal :tail, improper.tail
        assert_equal "", tail
        improper, tail = decode("\x83l\0\0\0\1jd\0\4tailtail")
        assert_equal ImproperList, improper.class
        assert_equal [[]], improper
        assert_equal Symbol, improper.tail.class
        assert_equal :tail, improper.tail
        assert_equal "tail", tail
    end

    def test_decode_small_tuple
        assert_raise(IncompleteData){decode("\x83h")}
        assert_raise(IncompleteData){decode("\x83h\1")}
        assert_equal Tuple, decode("\x83h\0")[0].class
        assert_equal [Tuple.new([]), ""], decode("\x83h\0")
        assert_equal [Tuple.new([]), "tail"], decode("\x83h\0tail")
        assert_equal [Tuple.new([[], []]), ""], decode("\x83h\2jj")
        assert_equal [Tuple.new([[], []]), "tail"], decode("\x83h\2jjtail")
    end

    def test_decode_large_tuple
        assert_raise(IncompleteData){decode("\x83i")}
        assert_raise(IncompleteData){decode("\x83i\0")}
        assert_raise(IncompleteData){decode("\x83i\0\0")}
        assert_raise(IncompleteData){decode("\x83i\0\0\0")}
        assert_raise(IncompleteData){decode("\x83i\0\0\0\1")}
        # Erlang use 'h' tag for small tuples
        assert_equal [Tuple.new([]), ""], decode("\x83i\0\0\0\0")
        assert_equal [Tuple.new([]), "tail"], decode("\x83i\0\0\0\0tail")
        assert_equal [Tuple.new([[], []]), ""], decode("\x83i\0\0\0\2jj")
        assert_equal [Tuple.new([[], []]), "tail"],
            decode("\x83i\0\0\0\2jjtail")
    end

    def test_decode_opaque_object
        opaque, tail = decode("\x83h\3d\0\x0f$erlport.opaqued\0\10language" \
            "m\0\0\0\4data")
        assert_equal OpaqueObject, opaque.class
        assert_equal "data", opaque.data
        assert_equal :language, opaque.language
        assert_equal "", tail
        opaque, tail = decode("\x83h\3d\0\x0f$erlport.opaqued\0\10language" \
            "m\0\0\0\4datatail")
        assert_equal OpaqueObject, opaque.class
        assert_equal "data", opaque.data
        assert_equal :language, opaque.language
        assert_equal "tail", tail
    end

    def test_decode_ruby_opaque_object
        opaque, tail = decode("\x83h\3d\0\x0f$erlport.opaqued\0\4ruby" \
            "m\0\0\0\10\4\b\"\ttest")
        assert_equal String, opaque.class
        assert_equal "test", opaque
        assert_equal "", tail
        opaque, tail = decode("\x83h\3d\0\x0f$erlport.opaqued\0\4ruby" \
            "m\0\0\0\10\4\b\"\ttesttail")
        assert_equal String, opaque.class
        assert_equal "test", opaque
        assert_equal "tail", tail
    end

    def test_decode_small_integer
        assert_raise(IncompleteData){decode("\x83a")}
        assert_equal [0, ""], decode("\x83a\0")
        assert_equal [0, "tail"], decode("\x83a\0tail")
        assert_equal [255, ""], decode("\x83a\xff")
        assert_equal [255, "tail"], decode("\x83a\xfftail")
    end

    def test_decode_integer
        assert_raise(IncompleteData){decode("\x83b")}
        assert_raise(IncompleteData){decode("\x83b\0")}
        assert_raise(IncompleteData){decode("\x83b\0\0")}
        assert_raise(IncompleteData){decode("\x83b\0\0\0")}
        # Erlang use 'a' tag for small integers
        assert_equal [0, ""], decode("\x83b\0\0\0\0")
        assert_equal [0, "tail"], decode("\x83b\0\0\0\0tail")
        assert_equal [2147483647, ""], decode("\x83b\x7f\xff\xff\xff")
        assert_equal [2147483647, "tail"], decode("\x83b\x7f\xff\xff\xfftail")
        assert_equal [-2147483648, ""], decode("\x83b\x80\0\0\0")
        assert_equal [-2147483648, "tail"], decode("\x83b\x80\0\0\0tail")
        assert_equal [-1, ""], decode("\x83b\xff\xff\xff\xff")
        assert_equal [-1, "tail"], decode("\x83b\xff\xff\xff\xfftail")
    end

    def test_decode_binary
        assert_raise(IncompleteData){decode("\x83m")}
        assert_raise(IncompleteData){decode("\x83m\0")}
        assert_raise(IncompleteData){decode("\x83m\0\0")}
        assert_raise(IncompleteData){decode("\x83m\0\0\0")}
        assert_raise(IncompleteData){decode("\x83m\0\0\0\1")}
        assert_equal ["", ""], decode("\x83m\0\0\0\0")
        assert_equal ["", "tail"], decode("\x83m\0\0\0\0tail")
        assert_equal ["data", ""], decode("\x83m\0\0\0\4data")
        assert_equal ["data", "tail"], decode("\x83m\0\0\0\4datatail")
    end

    def test_decode_float
        assert_raise(IncompleteData){decode("\x83F")}
        assert_raise(IncompleteData){decode("\x83F\0")}
        assert_raise(IncompleteData){decode("\x83F\0\0")}
        assert_raise(IncompleteData){decode("\x83F\0\0\0")}
        assert_raise(IncompleteData){decode("\x83F\0\0\0\0")}
        assert_raise(IncompleteData){decode("\x83F\0\0\0\0\0")}
        assert_raise(IncompleteData){decode("\x83F\0\0\0\0\0\0")}
        assert_raise(IncompleteData){decode("\x83F\0\0\0\0\0\0\0")}
        assert_equal [0.0, ""], decode("\x83F\0\0\0\0\0\0\0\0")
        assert_equal [0.0, "tail"], decode("\x83F\0\0\0\0\0\0\0\0tail")
        assert_equal [1.5, ""], decode("\x83F?\xf8\0\0\0\0\0\0")
        assert_equal [1.5, "tail"], decode("\x83F?\xf8\0\0\0\0\0\0tail")
    end

    def test_decode_small_big_integer
        assert_raise(IncompleteData){decode("\x83n")}
        assert_raise(IncompleteData){decode("\x83n\0")}
        assert_raise(IncompleteData){decode("\x83n\1\0")}
        # Erlang use 'a' tag for small integers
        assert_equal [0, ""], decode("\x83n\0\0")
        assert_equal [0, "tail"], decode("\x83n\0\0tail")
        assert_equal [6618611909121, ""], decode("\x83n\6\0\1\2\3\4\5\6")
        assert_equal [-6618611909121, ""], decode("\x83n\6\1\1\2\3\4\5\6")
        assert_equal [6618611909121, "tail"],
            decode("\x83n\6\0\1\2\3\4\5\6tail")
    end

    def test_decode_big_integer
        assert_raise(IncompleteData){decode("\x83o")}
        assert_raise(IncompleteData){decode("\x83o\0")}
        assert_raise(IncompleteData){decode("\x83o\0\0")}
        assert_raise(IncompleteData){decode("\x83o\0\0\0")}
        assert_raise(IncompleteData){decode("\x83o\0\0\0\0")}
        assert_raise(IncompleteData){decode("\x83o\0\0\0\1\0")}
        # Erlang use 'a' tag for small integers
        assert_equal [0, ""], decode("\x83o\0\0\0\0\0")
        assert_equal [0, "tail"], decode("\x83o\0\0\0\0\0tail")
        assert_equal [6618611909121, ""], decode("\x83o\0\0\0\6\0\1\2\3\4\5\6")
        assert_equal [-6618611909121, ""],
            decode("\x83o\0\0\0\6\1\1\2\3\4\5\6")
        assert_equal [6618611909121, "tail"],
            decode("\x83o\0\0\0\6\0\1\2\3\4\5\6tail")
    end

    def test_decode_compressed_term
        assert_raise(IncompleteData){decode("\x83P")}
        assert_raise(IncompleteData){decode("\x83P\0")}
        assert_raise(IncompleteData){decode("\x83P\0\0")}
        assert_raise(IncompleteData){decode("\x83P\0\0\0")}
        assert_raise(IncompleteData){decode("\x83P\0\0\0\0")}
        assert_raise(ValueError){decode("\x83P\0\0\0\x16" \
            "\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50")}
        assert_equal [[100] * 20, ""], decode("\x83P\0\0\0\x17" \
            "\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50")
        assert_equal [[100] * 20, "tail"], decode("\x83P\0\0\0\x17" \
            "\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50tail")
    end
end

class EncodeTestCase < Test::Unit::TestCase
    def test_decode
        assert_equal "\x83h\0", encode(Tuple.new([]))
        assert_equal "\x83h\2h\0h\0",
            encode(Tuple.new([Tuple.new([]), Tuple.new([])]))
        assert_equal "\x83h\xff" + "h\0" * 255,
            encode(Tuple.new([Tuple.new([])] * 255))
        assert_equal "\x83i\0\0\1\0" + "h\0" * 256,
            encode(Tuple.new([Tuple.new([])] * 256))
    end

    def test_encode_empty_list
        assert_equal "\x83j", encode([])
    end

    def test_encode_string_list
        assert_equal "\x83k\0\1\0", encode([0])
        r = (0..255).to_a
        assert_equal "\x83k\1\0" + r.pack("C*"), encode(r)
    end

    def test_encode_list
        assert_equal "\x83l\0\0\0\1jj", encode([[]])
        assert_equal "\x83l\0\0\0\5jjjjjj", encode([[], [], [], [], []])
    end

    def test_encode_improper_list
        assert_equal "\x83l\0\0\0\1h\0h\0",
            encode(ImproperList.new([Tuple.new([])], Tuple.new([])))
        assert_equal "\x83l\0\0\0\1a\0a\1", encode(ImproperList.new([0], 1))
    end

    def test_encode_atom
        assert_equal "\x83d\0\0", encode(EmptySymbol.new)
        assert_equal "\x83d\0\4test", encode(:test)
        assert_equal "\x83d\0\2\xd0\x90", encode([0x410].pack("U*").to_sym)
    end

    def test_encode_string
        assert_equal "\x83m\0\0\0\0", encode("")
        assert_equal "\x83m\0\0\0\4test", encode("test")
        assert_equal "\x83m\0\0\0\2\xd0\x90", encode([0x410].pack("U*"))
    end

    def test_encode_boolean
        assert_equal "\x83d\0\4true", encode(true)
        assert_equal "\x83d\0\5false", encode(false)
    end

    def test_encode_short_integer
        assert_equal "\x83a\0", encode(0)
        assert_equal "\x83a\xff", encode(255)
    end

    def test_encode_integer
        assert_equal "\x83b\xff\xff\xff\xff", encode(-1)
        assert_equal "\x83b\x80\0\0\0", encode(-2147483648)
        assert_equal "\x83b\0\0\1\0", encode(256)
        assert_equal "\x83b\x7f\xff\xff\xff", encode(2147483647)
    end

    def test_encode_long_integer
        assert_equal "\x83n\4\0\0\0\0\x80", encode(2147483648)
        assert_equal "\x83n\4\1\1\0\0\x80", encode(-2147483649)
        assert_equal "\x83o\0\0\1\0\0" + "\0" * 255 + "\1", encode(2 ** 2040)
        assert_equal "\x83o\0\0\1\0\1" + "\0" * 255 + "\1", encode(-2 ** 2040)
    end

    def test_encode_float
        assert_equal "\x83F\0\0\0\0\0\0\0\0", encode(0.0)
        assert_equal "\x83F?\xe0\0\0\0\0\0\0", encode(0.5)
        assert_equal "\x83F\xbf\xe0\0\0\0\0\0\0", encode(-0.5)
        assert_equal "\x83F@\t!\xfbM\x12\xd8J", encode(3.1415926)
        assert_equal "\x83F\xc0\t!\xfbM\x12\xd8J", encode(-3.1415926)
    end

    def test_encode_none
        assert_equal "\x83d\0\11undefined", encode(nil)
    end

    def test_encode_opaque_object
        assert_equal "\x83h\3d\0\x0f$erlport.opaqued\0\10language" \
            "m\0\0\0\4data", encode(OpaqueObject.new("data", \
                :language))
        assert_equal "\x83data",
            encode(OpaqueObject.new("data", :erlang))
    end

    def test_ruby_opaque_object
        assert_equal "\203h\003d\000\017$erlport.opaqued" \
            "\000\004rubym\000\000\000\004\004\b{\000", encode({})
        assert_raise(ValueError){encode(Module.new())}
    end

    def test_encode_compressed_term
        assert_equal "\x83l\x00\x00\x00\x05jjjjjj", encode([[]] * 5, true)
        assert_equal "\x83P\x00\x00\x00\x15" \
            "x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            encode([[]] * 15, true)
        assert_equal "\x83P\x00\x00\x00\x15" \
            "x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            encode([[]] * 15, 6)
        assert_equal "\x83P\x00\x00\x00\x15" \
            "x\xda\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            encode([[]] * 15, 9)
        assert_equal "\x83l\0\0\0\x0f" + "j" * 15 + "j",
            encode([[]] * 15, 0)
        assert_equal "\x83P\x00\x00\x00\x15" \
            "x\x01\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            encode([[]] * 15, 1)
    end
end
