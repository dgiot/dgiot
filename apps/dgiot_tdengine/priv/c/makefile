#--------------------------------------------------------------------
# Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
#--------------------------------------------------------------------

ROOT=./
TARGET=exe
LFLAGS = '-Wl,-rpath,/usr/local/taos/driver' -ltaos -lpthread -lm -lrt -lmosquitto
CFLAGS = -O3 -g -Wall -Wno-deprecated -fPIC -Wno-unused-result -Wconversion -Wno-char-subscripts -D_REENTRANT -Wno-format -D_REENTRANT -DLINUX -msse4.2 -Wno-unused-function -D_M_X64 -std=gnu99

all: $(TARGET)

exe:
	gcc $(CFLAGS) ./dgiot_tdengine_mqtt.c -o $(ROOT)/dgiot_tdengine_mqtt $(LFLAGS)

clean:
	rm $(ROOT)dgiot_tdengine_mqtt


