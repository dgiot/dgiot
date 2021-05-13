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

require "optparse"
require "ostruct"

require "erlport/erlproto"
require "erlport/erlang"


module ErlPort
class CommandLine
    def self.main
        options = parse
        port = ErlPort::ErlProto::Port.new(options.packet, options.stdio,
            options.compressed, nil, options.buffer_size)
        ErlPort::Erlang.start port
    end

    private

    def self.parse
        options = OpenStruct.new
        options.packet = 4
        options.stdio = true
        options.compressed = 0

        parser = OptionParser.new do |parser|
            parser.banner = "ErlPort - Erlang port protocol\n\n"

            parser.on("--packet N", Integer, "Message length sent in N bytes." \
                    " Valid values are 1, 2, or 4") {|n|
                raise OptionParser::InvalidArgument, \
                    "Valid values for --packet are 1, 2, or 4" \
                    if not (n == 1 or n == 2 or n == 4)
                options.packet = n
                }
            parser.on("--nouse_stdio", FalseClass, "Use file descriptors" \
                " 3 and 4 for communication with Erlang") {|f|
                options.stdio = f
                }
            parser.on("--use_stdio", TrueClass, "Use file descriptors" \
                " 0 and 1 for communication with Erlang") {|t|
                options.stdio = t
                }
            parser.on("--compressed N", Integer, "Compression level") {|level|
                raise OptionParser::InvalidArgument, \
                    "Valid values for --compressed are 0..9" \
                    if level < 0 or level > 9
                options.compressed = level
                }
            parser.on("--buffer_size S", Integer, "Receive buffer size") {|size|
                raise OptionParser::InvalidArgument, \
                    "Buffer size value should be greater than 0" \
                    if not size > 0
                options.buffer_size = size
                }
        end
        begin
            parser.parse!
        rescue OptionParser::ParseError => why
            print "Option error: #{why}\n"
            exit 2
        end
        options
    end
end
end

ErlPort::CommandLine.main
