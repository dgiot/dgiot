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

from erlport import Atom, erlang


# TODO: Base class should be extracted so we can use it for Ruby class too
class Python(object):

    def __init__(self, **kwargs):
        options = self.parse_options(kwargs)
        # TODO: Handle 'node' option with 'rpc' module
        result = erlang.call(Atom("python"), Atom("start"), options)
        if type(result) != tuple or len(result) != 2:
            # TODO: Fix exception
            raise Exception(result)
        if result[0] is not Atom("ok"):
            # TODO: Fix exception
            raise Exception(result[1])
        self.pid = result[1]

    def parse_options(self, kwargs):
        # TODO: Parse options
        return []

    def call(self, module, function, args):
        # TODO: Check all arguments
        # TODO: Reraise Python related exceptions
        return erlang.call(Atom("python"), Atom("call"),
            [self.pid, module, function, args])

    def stop(self):
        # TODO: Check result
        erlang.call(Atom("python"), Atom("stop"), [self.pid])
