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
require "set"

require "erlport/erlterms"
require "erlport/stdio"

include ErlPort::ErlTerm

require 'securerandom'


module ErlPort
module Erlang

    class ErlPortError < Exception
    end

    class InvalidMessage < ErlPortError
    end

    class UnknownMessage < ErlPortError
    end

    class UnexpectedMessage < ErlPortError
    end

    class UnexpectedResponses < UnexpectedMessage
    end

    class DuplicateMessageId < ErlPortError
    end

    class CallError < ErlPortError
        def initialize value
            if not value.is_a? Tuple or value.length != 4
                value = Tuple.new([nil, nil, value, []])
            end
            @language, @type, @value, @stacktrace = value
            super value.to_s
        end
    end

    module_function
    def cast pid, message
        # It's safe to call it from multiple threads because port.write will be
        # locked
        @@port.write(Tuple.new([:M, pid, message]))
        nil
    end

    module_function
    def call mod, function, args
        raise ValueError, mod \
            if not (mod.is_a? Symbol or mod.is_a? EmptySymbol)
        raise ValueError, function \
            if not (function.is_a? Symbol or function.is_a? EmptySymbol)
        raise ValueError, args if not args.is_a? Array
        _call mod, function, args, :N
    end

    module_function
    def self
        @@self = _call(:erlang, :self, [], :L) if @@self == nil
        @@self
    end

    module_function
    def make_ref
        _call(:erlang, :make_ref, [], :L)
    end

    module_function
    def set_default_encoder
        @@encoder = lambda {|v| v}
    end

    module_function
    def set_encoder &encoder
        check_handler encoder
        @@encoder = encoder
    end

    module_function
    def set_default_decoder
        @@decoder = lambda {|v| v}
    end

    module_function
    def set_decoder &decoder
        check_handler decoder
        @@decoder = decoder
    end

    module_function
    def set_default_message_handler
        @@handler = lambda {|v| v}
    end

    module_function
    def set_message_handler &handler
        check_handler handler
        @@handler = handler
    end

    module_function
    def start port
        setup port
        # Remove ErlPort::Erlang::start function
        Erlang.instance_eval {undef :start}
        begin
            self._receive
        rescue EOFError
        end
    end

    private

    module_function
    def setup port
        @@port = port
        @@self = nil
        @@responses = Responses.new
        @@message_id = MessageId.new
        ErlPort::StdIO::redirect port
        set_default_encoder
        set_default_decoder
        set_default_message_handler
    end

    module_function
    def check_handler handler
        raise ValueError, "expected single argument block: #{handler}" \
            if handler.arity != 1
    end

    module_function
    def _call mod, function, args, context
        response = @@message_id.generate {|mid|
            @@port.write(Tuple.new([:C, mid, mod, function,
                args.map(&@@encoder), context]))
            _receive(mid, false)
        }
        raise InvalidMessage, response if not response.is_a? Tuple \
            or response.length != 3
        mtype, _mid, value = response

        if mtype != :r
            raise CallError, value if mtype == :e
            raise UnknownMessage, response
        end
        @@decoder.call(value)
    end

    module_function
    def _receive expect_id=nil, expect_message=false
        marker = Object.new
        while true
            expected = @@responses.get(expect_id, marker)
            return expected if expected != marker
            message = @@port.read
            raise InvalidMessage, message \
                if not message.is_a? Tuple or message.length < 2
            case message[0]
                when :C
                    raise InvalidMessage, message if message.length != 5
                    mid, mod, function, args = message[1..-1]
                    call_with_error_handler(mid) {
                        incoming_call mid, mod, function, args
                    }
                when :M
                    return message if expect_message
                    raise InvalidMessage, message if message.length != 2
                    payload = message[1]
                    call_with_error_handler(nil) {
                        @@handler.call payload
                    }
                when :r, :e
                    expected = @@responses.put(expect_id, message, marker)
                    return expected if expected != marker
                else
                    raise UnknwonMessage, message
            end
        end
    end

    module_function
    def incoming_call mid, mod, function, args
        m = mod.to_s
        args = args.map(&@@decoder)
        f = function.to_s
        require m if m != ""
        idx = f.rindex("::")
        if idx == nil
            r = send(f, *args)
        else
            container = eval f[0...idx]
            fun = f[idx + 2..-1]
            r = container.send(fun, *args)
        end
        result = Tuple.new([:r, mid, @@encoder.call(r)])
        @@port.write(result)
    end

    module_function
    def call_with_error_handler mid
        begin
            yield
        rescue Exception => why
            exc = why.class.to_s.to_sym
            exc_tb = why.backtrace.reverse
            error = Tuple.new([:ruby, exc, why.message, exc_tb])
            if mid != nil
                result = Tuple.new([:e, mid, error])
            else
                result = Tuple.new([:e, error])
            end
            @@port.write(result)
        end
    end

    class Responses
        def initialize
            @responses = Hash.new
            @lock = Mutex.new
        end

        def get response_id, default=nil
            @lock.synchronize {
                if response_id == nil
                    raise UnexpectedResponses(@responses) \
                        if not @responses.empty?
                elsif @responses.member? response_id
                    return @responses.delete response_id
                end
            }
            default
        end

        def put response_id, message, default=nil
            raise UnexpectedMessage, message if response_id == nil
            @lock.synchronize {
                raise DuplicateMessageId, message \
                    if @responses.member? response_id
                raise InvalidMessage, message if not message.is_a? Tuple \
                    or message.length < 2
                return message if response_id == message[1]
                @responses[response_id] = message
            }
            default
        end
    end

    class MessageId
        def generate &code
            code.call SecureRandom.random_number(1 << 128)
        end
    end
end
end
