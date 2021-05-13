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

from inspect import getargspec
import sys
from sys import exc_info
from traceback import extract_tb
from threading import Lock
import uuid

from erlport import Atom


class Error(Exception):
    """ErlPort Error."""

class InvalidMessage(Error):
    """Invalid message."""

class UnknownMessage(Error):
    """Unknown message."""

class UnexpectedMessage(Error):
    """Unexpected message."""

class UnexpectedResponses(UnexpectedMessage):
    """Unexpected responses."""

class DuplicateMessageId(Error):
    """Duplicate message ID."""

class CallError(Error):
    """Call error."""

    def __init__(self, value):
        if type(value) != tuple or len(value) != 4:
            value = None, None, value, []
        self.language, self.type, self.value, self.stacktrace = value
        Error.__init__(self, value)

class Responses(object):

    def __init__(self):
        self.__responses = {}
        self.__lock = Lock()

    def get(self, response_id, default=None):
        with self.__lock:
            if response_id is None:
                if self.__responses:
                    raise UnexpectedResponses(self.__responses)
            elif response_id in self.__responses:
                return self.__responses.pop(response_id)
        return default

    def put(self, response_id, message, default=None):
        if response_id is None:
            raise UnexpectedMessage(message)
        with self.__lock:
            if response_id in self.__responses:
                raise DuplicateMessageId(message)
            try:
                if response_id == message[1]:
                    return message
            except IndexError:
                raise InvalidMessage(message)
            self.__responses[response_id] = message
        return default


class MessageHandler(object):

    def __init__(self, port):
        self.port = port
        self.set_default_encoder()
        self.set_default_decoder()
        self.set_default_message_handler()
        self._self = None
        self.responses = Responses()

    def new_message_id(self):
        return uuid.uuid4().int

    def set_default_encoder(self):
        self.encoder = lambda o: o

    def set_encoder(self, encoder):
        self._check_handler(encoder)
        self.encoder = encoder

    def set_default_decoder(self):
        self.decoder = lambda o: o

    def set_decoder(self, decoder):
        self._check_handler(decoder)
        self.decoder = decoder

    def set_default_message_handler(self):
        self.handler = lambda o: None

    def set_message_handler(self, handler):
        self._check_handler(handler)
        self.handler = handler

    def _check_handler(self, handler):
        # getargspec will raise TypeError if handler is not a function
        args, varargs, _keywords, defaults = getargspec(handler)
        largs = len(args)
        too_much = largs > 1 and largs - len(default) > 1
        too_few = largs == 0 and varargs is None
        if too_much or too_few:
            raise ValueError("expected single argument function: %r"
                % (handler,))

    def start(self):
        try:
            self._receive()
        except EOFError:
            pass

    def _receive(self, expect_id=None, expect_message=False):
        marker = object()
        while True:
            expected = self.responses.get(expect_id, marker)
            if expected is not marker:
                return expected
            message = self.port.read()
            try:
                mtype = message[0]
            except (IndexError, TypeError):
                raise InvalidMessage(message)

            if mtype == b"C":
                try:
                    mid, module, function, args = message[1:]
                except ValueError:
                    raise InvalidMessage(message)
                self._call_with_error_handler(
                    mid, self._incoming_call, mid, module, function, args)
            elif mtype == b"M":
                if expect_message:
                    return message
                try:
                    payload, = message[1:]
                except ValueError:
                    raise InvalidMessage(message)
                self._call_with_error_handler(None, self.handler, payload)
            elif mtype == b"r" or mtype == b"e":
                expected = self.responses.put(expect_id, message, marker)
                if expected is not marker:
                    return expected
            else:
                raise UnknownMessage(message)

    def cast(self, pid, message):
        # It's safe to call it from multiple threads because port.write will be
        # locked
        self.port.write((Atom(b'M'), pid, message))

    def call(self, module, function, args):
        if not isinstance(module, Atom):
            raise ValueError(module)
        if not isinstance(function, Atom):
            raise ValueError(function)
        if not isinstance(args, list):
            raise ValueError(args)
        return self._call(module, function, args, Atom(b'N'))

    def self(self):
        if self._self is None:
            self._self = self._call(Atom(b'erlang'), Atom(b'self'), [],
                Atom(b'L'))
        return self._self

    def make_ref(self):
        return self._call(Atom(b'erlang'), Atom(b'make_ref'), [], Atom(b'L'))

    def _call(self, module, function, args, context):
        mid = self.new_message_id()
        self.port.write((Atom(b'C'), mid, module, function,
                         # TODO: Optimize list(map())
                         list(map(self.encoder, args)), context))

        response = self._receive(expect_id=mid)

        try:
            mtype, _mid, value = response
        except ValueError:
            raise InvalidMessage(response)
        if mtype != b"r":
            if mtype == b"e":
                raise CallError(value)
            raise UnknownMessage(response)
        return self.decoder(value)

    def _incoming_call(self, mid, module, function, args):
        mod = module.decode()
        objects = function.decode().split(".")
        f = sys.modules.get(mod)
        if not f:
            f = __import__(mod, {}, {}, [objects[0]])
        for o in objects:
            f = getattr(f, o)
        result = Atom(b"r"), mid, self.encoder(f(*map(self.decoder, args)))
        self.port.write(result)

    def _call_with_error_handler(self, mid, function, *args):
        try:
            function(*args)
        except:
            t, val, tb = exc_info()
            exc = Atom(bytes("%s.%s" % (t.__module__, t.__name__), "utf-8"))
            exc_tb = extract_tb(tb)
            exc_tb.reverse()
            error = Atom(b"python"), exc, str(val), exc_tb
            if mid is not None:
                result = Atom(b"e"), mid, error
            else:
                result = Atom(b"e"), error
            self.port.write(result)

def setup_api_functions(handler):
    global call, cast, self, make_ref
    global set_default_encoder, set_default_decoder
    global set_default_message_handler
    global set_encoder, set_decoder, set_message_handler
    call = handler.call
    cast = handler.cast
    self = handler.self
    make_ref = handler.make_ref
    set_encoder = handler.set_encoder
    set_decoder = handler.set_decoder
    set_message_handler = handler.set_message_handler
    set_default_encoder = handler.set_default_encoder
    set_default_decoder = handler.set_default_decoder
    set_default_message_handler = handler.set_default_message_handler

def setup(port):
    from . import stdio
    global MessageHandler, setup
    handler = MessageHandler(port)
    setup_api_functions(handler)
    stdio.redirect(port)
    del MessageHandler, setup
    handler.start()
