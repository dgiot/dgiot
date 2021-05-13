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

require "thread"

require "erlport/errors"
require "erlport/erlterms"

module ErlPort
module ErlProto
    class Port
        attr_reader :in_d, :out_d

        @@formats = {
            1 => "C",
            2 => "n",
            4 => "N",
            }

        def initialize packet=4, use_stdio=true, compressed=false, \
                descriptors=nil, buffer_size=65536
            raise ValueError, "invalid buffer size value: #{buffer_size}" \
                if buffer_size < 1
            @format = @@formats[packet]
            raise ValueError, "invalid packet size value: #{packet}" \
                if @format == nil
            if descriptors != nil
                @in_d, @out_d = descriptors
            elsif use_stdio
                @in_d, @out_d = 0, 1
            else
                @in_d, @out_d = 3, 4
            end

            @in = IO.new(@in_d, "rb").binmode
            @out = IO.new(@out_d, "wb").binmode

            @packet = packet
            @compressed = compressed
            @buffer = ""
            @buffer_size = buffer_size

            @read_lock = Mutex.new
            @write_lock = Mutex.new
        end

        def read
            packet = @packet
            buffer = @buffer
            @read_lock.synchronize {
                while buffer.length < packet
                    buffer += read_data()
                end
                length = unpack(buffer[0...packet]) + packet
                while buffer.length < length
                    buffer += read_data()
                end
                term, @buffer = ErlPort::ErlTerm.decode(buffer[packet..-1])
                term
            }
        end

        def write message
            data = ErlPort::ErlTerm.encode(message, compressed=@compressed)
            length = data.length
            data = pack(length) + data
            @write_lock.synchronize {
                while data != ""
                    begin
                        n = @out.syswrite(data)
                    rescue Errno::EPIPE, Errno::EINVAL
                        raise EOFError, "end of file reached"
                    end
                    raise EOFError, "end of file reached" if n == 0
                    data = data[n..-1]
                end
            }
            length + @packet
        end

        def close
            @in.close
            @out.close
        end

        private

        def unpack string
            string.unpack(@format)[0]
        end

        def pack value
            [value].pack(@format)
        end

        def read_data
            begin
                buf = @in.sysread(@buffer_size)
            rescue Errno::EPIPE, Errno::EINVAL
                raise EOFError, "end of file reached"
            end
            raise EOFError, "end of file reached" if buf == nil or buf == ""
            buf
        end
    end
end
end
