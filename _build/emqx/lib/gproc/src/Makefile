## The MIT License
##
## Copyright (c) 2008 Ulf Wiger <ulf@wiger.net>,
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
## THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.


.SUFFIXES: .erl .beam

APP = gproc

vpath %.beam ../ebin
vpath %.pdf ../doc

ERLC = erlc -W -I ../include -o ../ebin
ERLC += +debug_info

#EQC ?= /host/dev/eqc-1.16

EQC_ERLC = $(ERLC) -pa $(EQC)/ebin -I ../include -DEQC
EQC_ERL = erl -pz ../ebin -pz $(EQC)/ebin -pz ../test/eqc


SOURCES = $(wildcard *.erl)

beams = gproc.beam \
	gproc_app.beam \
	gproc_dist.beam \
	gproc_lib.beam \
	gproc_sup.beam

all : $(beams)
clean :
	rm -f  *~ ../ebin/*.beam ../test/eqc/*.beam

## erlc targets
$(beams) : %.beam : %.erl
	$(ERLC) $<

../test/eqc/gproc_eqc_tests.beam : ../test/eqc/gproc_eqc_tests.erl
	$(EQC_ERLC) $<

../test/eqc/gproc_pulse.beam : ../test/eqc/gproc_pulse.erl
	$(EQC_ERLC) $<

doc: ../doc/edoc-info

../doc/edoc-info: ../doc/overview.edoc $(SOURCES)
	erl -noshell -eval 'edoc:application($(APP),"..",[])' -s init stop

## test targets
eunit: all 
	erl -noshell -boot start_clean -sasl errlog_type error \
	-pa ../ebin -eval 'eunit:test("../ebin", [verbose])' -s init stop

eqc : all ../test/eqc/gproc_eqc_tests.beam
	$(EQC_ERL) -s gproc_eqc_tests run -s erlang halt

itest : all ../test/eqc/gproc_eqc_tests.beam
	$(EQC_ERL) -s eqc start

pulsetest : all ../test/eqc/gproc_pulse.beam
	$(EQC_ERL) -pa ../../scheduler_msg -pa -s eqc start 