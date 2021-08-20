# Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

from emqx.exproto.core import *
from emqx.exproto.abstract_handler import AbstractExProtoHandler
from emqx.exproto.connection import Connection, ConnectionInfo

class SdkDemo(AbstractExProtoHandler):
    def on_connect(self, connection: Connection, connection_info: ConnectionInfo):
        print(connection)
        print(connection_info)
        self.subscribe(connection, b"t/dn", 0)

    def on_received(self, connection: Connection, data: bytes, state: any):
        print(connection)
        self.send(connection, data)

    def on_terminated(self, connection: Connection, reason: str, state: any):
        print(connection)
        print(reason)

    def on_deliver(self, connection: Connection, message_list: list):
        print(connection)
        for message in message_list:
            print(message)

driver.exproto_driver = SdkDemo()
