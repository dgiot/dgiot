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

RELDIR = ebin
TESTDIR = .eunit

VERSIONS := $(shell ./get_versions)
HEADERS = $(wildcard src/*.hrl)
SOURCES = $(wildcard src/*.erl)
BEAMS = $(patsubst src/%.erl,$(RELDIR)/%.beam,$(SOURCES))
LANGTESTDIRS = $(patsubst %,test/%,$(VERSIONS))
LANGTESTSOURCES = $(foreach dir,$(LANGTESTDIRS),$(wildcard $(dir)/*.erl))
TESTSOURCES = $(wildcard test/*.erl)
TESTBEAMS = $(patsubst src/%.erl,$(TESTDIR)/%.beam,$(SOURCES)) \
    $(patsubst test/%.erl,$(TESTDIR)/%.beam,$(TESTSOURCES)) \
    $(patsubst %.erl,$(TESTDIR)/%.beam,$(notdir $(LANGTESTSOURCES)))
ERLC = erlc -Wall +warnings_as_errors -I include -I src
ERL = erl -noinput -pa ../erlport


compile: compile-priv $(BEAMS)

compile-test: compile-priv $(TESTDIR) $(TESTDIR)/erlport.app $(TESTBEAMS)

compile-priv:
	@for folder in $$(ls -1 priv); do \
		(cd priv/$$folder; $(MAKE)) \
	done
	@mkdir -p ebin

$(RELDIR)/%.beam: src/%.erl $(HEADERS)
	$(ERLC) +debug_info -o $(RELDIR) $<

$(TESTDIR)/%.beam: test/%.erl
	$(ERLC) +debug_info -o $(TESTDIR) $<

$(TESTDIR)/%.beam: test/*/%.erl
	$(ERLC) +debug_info -o $(TESTDIR) $<

$(TESTDIR)/%.beam: src/%.erl $(HEADERS)
	$(ERLC) +debug_info -o $(TESTDIR) $<

$(TESTDIR):
	@mkdir $@

$(TESTDIR)/erlport.app:
	@ln ebin/erlport.app $(TESTDIR)

test: erlang-test
	@for folder in $(VERSIONS); do \
		echo; \
		echo "====================== Test $$folder ========================="; \
		(cd priv/$$folder; $(MAKE) test) \
	done

test-verbose: erlang-test-verbose
	@for folder in $(VERSIONS); do \
		echo; \
		echo "====================== Test $$folder ========================="; \
		(cd priv/$$folder; $(MAKE) test-verbose) \
	done

erlang-test: compile-test
	@./print_versions
	@echo
	@echo "====================== Test erlang =========================="
	@./runtest

erlang-test-verbose: compile-test
	@./print_versions
	@echo
	@echo "====================== Test erlang =========================="
	@./runtest verbose

check: $(TESTDIR) $(TESTBEAMS)
	@dialyzer $(TESTDIR) | fgrep -v -f dialyzer.ignore

create-ignore-file: $(TESTDIR) $(TESTBEAMS)
	@dialyzer $(TESTDIR) | egrep '^[^:]+:[0-9]+: ' > dialyzer.ignore; exit 0

doc:
	@$(ERL) -eval 'edoc:application(erlport)' -s init stop

clean: erlang-clean priv-clean doc-clean

priv-clean:
	@for folder in $$(ls -1 priv); do \
		(cd priv/$$folder; $(MAKE) clean) \
	done

doc-clean:
	@rm -f doc/*.html doc/*.png doc/*.css doc/edoc-info

erlang-clean:
	@rm -rf $(RELDIR)/*.beam $(TESTDIR)
	@find test \( -name '*.py[co]' -o -name '__pycache__' \) -delete

release: clean compile
	@./release bin

release-src: clean
	@./release src


.PHONY: compile compile-test test test-verbose check doc clean compile-priv
.PHONY: create-ignore-file priv-clean erlang-test erlang-test-verbose
.PHONY: erlang-clean release release-src
