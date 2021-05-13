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

#
# Erlang external term format.
#
# See Erlang External Term Format for details:
#    http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
#

require "zlib"
require "erlport/errors"

module ErlPort
module ErlTerm
    class IncompleteData < Exception
        def initialize string
            super "incomplete data: '#{string}'"
        end
    end

    class Tuple
        def initialize array
            raise TypeError, "array expected" if not array.is_a? Array
            @data = array
        end

        def == other
            self.class == other.class and self.to_a == other.to_a
        end

        def === other
            self == other
        end

        def eql? other
            self == other
        end

        def hash
            [self.class, self.to_a].hash
        end

        def to_s
            self.inspect
        end

        def inspect
            "Tuple(#{@data.inspect})"
        end

        def to_a
            @data
        end

        def to_ary
            @data
        end

        def [] index
            @data[index]
        end

        def length
            @data.length
        end
    end

    class ImproperList < Array
        attr_reader :tail

        def initialize array, tail
            raise ValueError, "empty list not allowed" if array.empty?
            raise TypeError, "non list object expected for tail" \
                if tail.is_a? Array
            @tail = tail
            super array
        end

        def == other
            self.class == other.class and self.to_a == other.to_a \
                and self.tail == other.tail
        end

        def === other
            self == other
        end

        def eql? other
            self == other
        end

        def hash
            [self.class, self.to_a, self.tail].hash
        end
    end

    class OpaqueObject
        attr_reader :data
        attr_reader :language

        MARKER = :"$erlport.opaque"

        def initialize data, language
            raise TypeError, "data must be instance of String" \
                if not data.is_a? String
            raise TypeError, "language must be instance of Symbol" \
                if not language.is_a? Symbol
            @data = data
            @language = language
        end

        def self.decode data, language
            return Marshal.load(data) if language == :ruby
            return OpaqueObject.new data, language
        end

        def encode
            return @data if @language == :erlang
            return encode_term(Tuple.new([MARKER, @language, @data]))
        end

        def == other
            self.class == other.class and self.language == other.language \
                and self.data == other.data
        end

        def === other
            self == other
        end

        def eql? other
            self == other
        end

        def hash
            [self.class, self.language, self.data].hash
        end
    end

    module_function
    def decode s
        string = s.dup.force_encoding("BINARY")
        raise IncompleteData, string if string == ""
        raise ValueError, "unknown protocol version: %s" % string[0] \
            if string[0] != "\x83"
        if string[1] == "P"
            raise IncompleteData, string if string.bytesize < 16
            zstream = Zlib::Inflate.new
            term_string = zstream.inflate(string[6..-1])
            unused = zstream.finish
            zstream.close
            uncompressed_size = string[2,4].unpack("N")[0]
            raise ValueError, "invalid compressed tag, #{uncompressed_size}" \
                " bytes but got #{term_string.bytesize}" \
                if term_string.bytesize != uncompressed_size
            # tail data returned by decode_term() can be simple ignored
            term, _tail = decode_term term_string
            return term, unused
        end
        decode_term string[1..-1]
    end

    module_function
    def encode term, compressed=false
        encoded_term = encode_term term
        if compressed == true
            # default compression level is 6
            compressed = 6
        elsif not compressed.is_a? Integer
            compressed = 0
        elsif compressed < 0 or compressed > 9
            raise ValueError, "invalid compression level: #{compressed}"
        end
        if compressed > 0
            zlib_term = Zlib::Deflate.deflate(encoded_term, compressed)
            ln = encoded_term.bytesize
            if zlib_term.bytesize + 5 <= ln
                # Compressed term should be smaller
                return [131, 80, ln].pack("CCN") + zlib_term
            end
        end
        "\x83" + encoded_term
    end

    private

    module_function
    def decode_term string
        raise IncompleteData, string if string == ""
        tag = string[0]
        case tag
            when "d"
                # ATOM_EXT
                ln = string.bytesize
                raise IncompleteData, string if ln < 3
                length = string[1,2].unpack("n")[0] + 3
                raise IncompleteData, string if ln < length
                name = string[3...length]
                case name
                    when "true"
                        return true, string[length..-1]
                    when "false"
                        return false, string[length..-1]
                    when "undefined"
                        return nil, string[length..-1]
                end
                return name.to_sym, string[length..-1]
            when "j"
                # NIL_EXT
                return [], string[1..-1]
            when "k"
                # STRING_EXT
                ln = string.bytesize
                raise IncompleteData, string if ln < 3
                length = string[1,2].unpack("n")[0] + 3
                raise IncompleteData, string if ln < length
                return string[3...length].unpack("C*"), string[length..-1]
            when "l", "h", "i"
                if tag == "h"
                    raise IncompleteData, string if string.bytesize < 2
                    length = string[1].ord
                    tail = string[2..-1]
                else
                    raise IncompleteData, string if string.bytesize < 5
                    length = string[1,4].unpack("N")[0]
                    tail = string[5..-1]
                end
                lst = []
                while length > 0
                    term, tail = decode_term(tail)
                    lst.push(term)
                    length -= 1
                end
                if tag == "l"
                    raise IncompleteData, string if tail == ""
                    if tail[0] != "j"
                        improper_tail, tail = decode_term(tail)
                        return ImproperList.new(lst, improper_tail), tail
                    end
                    return lst, tail[1..-1]
                end
                return [OpaqueObject.decode(lst[2], lst[1]), tail] \
                    if lst.length == 3 and lst[0] == OpaqueObject::MARKER
                return Tuple.new(lst), tail
            when "a"
                # SMALL_INTEGER_EXT
                raise IncompleteData, string if string.bytesize < 2
                return string[1].ord, string[2..-1]
            when "b"
                # INTEGER_EXT
                raise IncompleteData, string if string.bytesize < 5
                # Ruby 1.9.[12] don't support "l>"
                int = string[1,4].unpack("N")[0]
                # Turn unsigned integer to signed integer
                int -= 0x100000000 if int > 0x7fffffff
                return int, string[5..-1]
            when "m"
                # BINARY_EXT
                ln = string.bytesize
                raise IncompleteData, string if ln < 5
                length = string[1,4].unpack("N")[0] + 5
                raise IncompleteData, string if ln < length
                return string[5...length], string[length..-1]
            when "F"
                # NEW_FLOAT_EXT
                raise IncompleteData, string if string.bytesize < 9
                return string[1,8].unpack("G")[0], string[9..-1]
            when "n", "o"
                # SMALL_BIG_EXT, LARGE_BIG_EXT
                if tag == "n"
                    raise IncompleteData, string if string.bytesize < 3
                    length, sign = string[1,2].unpack("CC")
                    tail = string[3..-1]
                else
                    raise IncompleteData, string if string.bytesize < 6
                    length, sign = string[1,5].unpack("NC")
                    tail = string[6..-1]
                end
                raise IncompleteData, string if tail.bytesize < length
                n = 0
                if length > 0
                    for i in tail[0,length].unpack("C*").reverse!
                        n = (n << 8) | i
                    end
                    n = -n if sign != 0
                end
                return n, tail[length..-1]
        end

        raise ValueError, "unsupported data: '%s'" % string
    end

    module_function
    def encode_term term
        case term
            when Tuple
                arity = term.length
                if arity <= 255
                    header = "h#{arity.chr}"
                elsif arity <= 4294967295
                    header = [105, arity].pack("CN")
                else
                    raise ValueError, "invalid tuple arity: #{arity}"
                end
                return header + term.to_a.map{|i| encode_term i}.join
            # Should be before Array
            when ImproperList
                length = term.length
                raise ValueError, "invalid improper list length: #{length}" \
                    if length > 4294967295
                header = [108, length].pack("CN")
                return header + term.map{|i| encode_term i}.join \
                    + encode_term(term.tail)
            when Array
                length = term.length
                if term.empty?
                    return "j"
                elsif length <= 65535
                    if term.index{|i| not i.is_a? Integer or i > 255 or i < 0} == nil
                        return [107, length].pack("Cn") + term.pack("C*")
                    end
                elsif length > 4294967295
                    raise ValueError, "invalid list length: #{length}"
                end
                return [108, length].pack("CN") \
                    + term.map{|i| encode_term i}.join + "j"
            when Symbol
                s = term.to_s.force_encoding("BINARY")
                length = s.bytesize
                raise ValueError, "invalid atom length: #{length}" \
                    if length > 255
                return [100, length].pack("Cn") + s
            when String
                # term can be a frozen String here so it should be duplicated
                s = term.dup.force_encoding("BINARY")
                length = s.bytesize
                raise ValueError, "invalid binary length: #{length}" \
                    if length > 4294967295
                return [109, length].pack("CN") + s
            when true
                return "d\0\4true"
            when false
                return "d\0\5false"
            when Integer
                if term >= 0 and term <= 255
                    return "a#{term.chr}"
                elsif term >= -2147483648 and term <= 2147483647
                    return [98, term].pack("CN")
                end

                if term >= 0
                    sign = 0
                else
                    sign = 1
                    term = -term
                end

                bytes = []
                while term != 0
                    bytes.push(term & 0xff)
                    term >>= 8
                end

                length = bytes.length
                if length <= 255
                    return [110, length, sign].pack("CCC") + bytes.pack("C*")
                elsif length <= 4294967295
                    return [111, length, sign].pack("CNC") + bytes.pack("C*")
                end
                raise ValueError, "invalid integer value with length #{length}"
            when Float
                return [70, term].pack("CG")
            when nil
                return "d\0\11undefined"
            when OpaqueObject
                return term.encode
        end

        begin
            data = Marshal.dump(term)
        rescue
            raise ValueError, "unsupported data type: #{term.class}"
        end
        return OpaqueObject.new(data, :ruby).encode()
    end
end
end
