#
# Makefile for gpb -- for projects that use (GNU) make to build
#
# Copyright (C) 2013  Tomas Abrahamsson
#
# Author: Tomas Abrahamsson <tab@lysator.liu.se>
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA  02110-1301  USA


# Parameters:
# ----------
# GPB_PREFIX      -- if this makefile is to be included in some larger project
#                    NB: must end with a slash!
# VERBOSE         -- set (to any value) to make the following steps verbose:
#                    * eunit testing
#                    * generation of the include/gpb_version.hrl and 
#                      ebin/gpb.app files
#                    * edoc generation command
#                    * xref checking command
# ERL             -- path to erl
# ERLC            -- path to erlc
# ERLC_FLAGS      -- any additional flags to erlc
# EDOC_DEST_DIR   -- destination directory for documentation
# EDOC_OPTS       -- any options to edoc (NB: _not_ including list brackets!)
# ERL_BATCH_FLAGS -- Flags to start the Erlang VM in batch mode


.SUFFIXES += .erl .xrl .yrl .hrl .beam .proto .plt
.PHONY: all clean test doc xref dialyze build_plt clean_plt

SHELL = /bin/sh

ERL  ?= erl
ERLC ?= erlc

GPB_PREFIX =

src            = $(GPB_PREFIX)src
ebin           = $(GPB_PREFIX)ebin
incdir         = $(GPB_PREFIX)include
descr_src      = $(GPB_PREFIX)descr_src
test           = $(GPB_PREFIX)test
priv           = $(GPB_PREFIX)priv
doc            = $(GPB_PREFIX)doc
build          = $(GPB_PREFIX)build

empty_str :=
space := $(empty_str) $(empty_str)
comma := ,

##
ifndef EDOC_DEST_DIR
EDOC_DEST_DIR=.
endif

ifndef EDOC_OPTS
EDOC_OPTS={preprocess,true},{pretty_printer,erl_pp}
endif

## Check verbosity
ifdef VERBOSE
verbose_opt := verbose
silencer    :=
else
verbose  :=
silencer := @
endif

# When making 'clean', don't run in parallel.
# Otherwise "make -j clean all" in an already-built dir will fail:
# it will clean while rebuilding, or fail to detect what needs to
# be rebuilt since it runs in parallel.
#
# If anyone tries that, revert to non-parallel execution, which
# is safe, but slower. Issue a warning about that.
ifneq ($(filter clean,$(MAKECMDGOALS)),)
.NOTPARALLEL:
ifneq ($(filter-out clean,$(MAKECMDGOALS)),)
# User is not _only_ making clean...
$(warning "WARNING: cannot make in parallel when _also_ making clean")
endif
endif


ERLC_FLAGS += -Wall +debug_info -I$(incdir)

ERL_BATCH_FLAGS = +B -noshell -noinput

OTP_MAJOR_MINOR = $(shell $(ERL) $(ERL_BATCH_FLAGS) -eval ' \
    GetVsnFromFile = \
        fun(V) -> case lists:reverse(string:strip(V,right,$$\n)) of \
                      "**"++Rest -> lists:reverse(Rest); \
                      Rest -> lists:reverse(Rest) \
                  end \
        end, \
    RootDir = code:root_dir(), \
    io:format( \
      "~s~n", \
      [case erlang:system_info(otp_release) of \
           "R"++_=Rel -> Rel; \
           RStr -> \
               lists:foldl( \
                 fun(_,R) when R /= RStr -> R; \
                    (F,R) -> case file:read_file(F) of \
                                 {ok,B} -> GetVsnFromFile(binary_to_list(B));\
                                 _ -> R \
                             end \
                 end, \
                 RStr, \
                 [filename:join([RootDir,"releases",RStr,"OTP_VERSION"]), \
                  filename:join([RootDir,"releases","OTP_VERSION"])]) \
       end]), \
    halt(0).')

# Use the same plt file as rebar would use, for compatibility
plt = $(GPB_PREFIX).gpb-$(OTP_MAJOR_MINOR).plt


ifdef NO_HAVE_MAPS
ERLC_FLAGS += -DNO_HAVE_MAPS=true
else
## attempt to auto-detect
ERLVM_SUPPORTS_MAPS := $(shell $(ERL) $(ERL_BATCH_FLAGS) -eval ' \
                             try maps:size(maps:new()) of \
                                0 -> io:format("true~n") \
                             catch error:undef -> io:format("false~n") \
                             end, \
                             receive after 10 -> ok end.' \
                         -s erlang halt)
ifeq ($(ERLVM_SUPPORTS_MAPS),false)
ERLC_FLAGS += -DNO_HAVE_MAPS=true
endif
endif

ifdef NO_HAVE_RAND
ERLC_FLAGS += -DNO_HAVE_RAND=true
else
## attempt to auto-detect
ERL_HAS_RAND := $(shell $(ERL) $(ERL_BATCH_FLAGS) -eval ' \
                             try rand:uniform() of \
                                F when is_float(F) -> io:format("true~n") \
                             catch error:undef -> io:format("false~n") \
                             end, \
                             receive after 10 -> ok end.' \
                         -s erlang halt)
ifeq ($(ERL_HAS_RAND),false)
ERLC_FLAGS += -DNO_HAVE_RAND=true
endif
endif

ifdef NO_HAVE_ERL20_STR_FUNCTIONS
ERLC_FLAGS += -DNO_HAVE_ERL20_STR_FUNCTIONS=true
else
## attempt to auto-detect
ERL_HAS_ERL20_STR_FUNCTIONS := $(shell $(ERL) $(ERL_BATCH_FLAGS) -eval ' \
                             try string:find("abc", "b") of \
                                "bc" -> io:format("true~n") \
                             catch error:undef -> io:format("false~n") \
                             end, \
                             receive after 10 -> ok end.' \
                         -s erlang halt)
ifeq ($(ERL_HAS_ERL20_STR_FUNCTIONS),false)
ERLC_FLAGS += -DNO_HAVE_ERL20_STR_FUNCTIONS=true
endif
endif


# Sorting it also eliminates duplicates
# (eg: gpb_parse due to both .yrl and .erl on rebuild, ditto for gpb_scan)
MODULES := \
	$(sort \
	  $(patsubst $(src)/%.erl,%,$(wildcard $(src)/*.erl)) \
	  $(patsubst $(src)/%.yrl,%,$(wildcard $(src)/*.yrl)) \
	  $(patsubst $(src)/%.xrl,%,$(wildcard $(src)/*.xrl)))

DESCR_PROTO := $(priv)/proto3/google/protobuf/descriptor.proto
DESCR_PROTO_ERL := $(descr_src)/gpb_descriptor.erl
DESCR_PROTO_HRL := $(descr_src)/gpb_descriptor.hrl

DESCR_MODULES := \
	gpb_compile_descr \
	$(patsubst $(priv)/proto3/google/protobuf/%.proto,gpb_%,$(DESCR_PROTO))

TEST_MODULES := \
	$(patsubst $(test)/%.erl,%,$(wildcard $(test)/*.erl)) \
	gpb_compile_descr_tests

# Run eunit on these modules:
# - If module M and M_tests exist, only include M (M_tests is then implicit)
# - If M_tests exists, but no M, include M_tests (eg gpb_compile_maps_tests)
# sorting it also removes any duplicates
EUNIT_MODULES := \
	$(MODULES) \
	$(filter-out $(patsubst %,%_tests,$(MODULES)),$(TEST_MODULES))


BEAMS       := $(patsubst %,$(ebin)/%.beam,$(MODULES))
DESCR_BEAMS := $(patsubst %,$(ebin)/%.beam,$(DESCR_MODULES))
TEST_BEAMS  := $(patsubst %,$(test)/%.beam,$(TEST_MODULES)) \
               $(test)/gpb_compile_maps_tests.beam

TARGETS = \
	$(incdir)/gpb_version.hrl \
	$(BEAMS) \
	$(DESCR_BEAMS) \
	$(ebin)/gpb.app


all:	$(TARGETS)

clean:
	$(RM) $(TARGETS)
	$(RM) $(src)/gpb_parse.erl
	$(RM) $(src)/gpb_scan.erl
	$(RM) $(DESCR_PROTO_ERL)
	$(RM) $(DESCR_PROTO_HRL)
	$(RM) $(TEST_BEAMS)
	$(RM) -r doc/*.html doc/*.png doc/*.css doc/edoc-info doc/html

test:	all $(TEST_BEAMS) FORCE
	@echo Testing...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -pa $(test) -pa $(ebin) -eval " \
	    case eunit:test([$(subst $(space),$(comma),$(EUNIT_MODULES))], \
			    [$(verbose_opt)]) of \
		ok -> halt(0); \
		_  -> halt(1) \
	    end."

doc:	| $(src)/gpb_parse.erl $(src)/gpb_scan.erl
	@echo Generating documentation...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -pa $(ebin) -eval " \
	    case edoc:application(gpb,\"$(EDOC_DEST_DIR)\",[$(EDOC_OPTS)]) of \
		ok -> halt(0); \
		_  -> halt(1) \
	    end."

doc-check: doc
	$(silencer)tidy -config .tidyrc -q -e doc/gpb_*.html 2>&1 | \
	    egrep -v '(trimming empty|inserting implicit) <p>' | \
	    egrep -v '<table> lacks "summary" attribute' || :

xref: all
	@echo Checking for calls to undefined functions...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -eval " \
	    Res = xref:d(\"$(ebin)\"), \
	    case lists:keyfind(undefined,1,Res) of \
		{undefined,[]}     -> halt(0); \
		{undefined,Undefs} -> io:format(\"~p~n\",[Undefs]), halt(1) \
	    end."

dialyze: all $(plt)
	dialyzer -q --plt $(plt) -r $(ebin)

build_plt: $(plt)

clean_plt:
	$(RM) -f $(plt)

$(plt):
	dialyzer -q --build_plt --output_plt $@ \
		--apps erts kernel stdlib syntax_tools


FORCE:

##
## General default rules for how to compile some files
##
$(ebin)/%.beam: $(src)/%.erl | $(ebin)
	$(ERLC) $(ERLC_FLAGS) -pa $(ebin) -o $(ebin) $<

$(ebin)/%.beam: $(descr_src)/%.erl | $(ebin)
	$(ERLC) $(ERLC_FLAGS) -Iinclude -pa $(ebin) -o $(ebin) $<

$(test)/%.beam: $(test)/%.erl | $(ebin)
	$(ERLC) $(ERLC_FLAGS) $(EUNIT_ERLC_FLAGS) -pa $(ebin) -o $(test) $<

$(test)/%.beam: $(descr_src)/%.erl | $(ebin)
	$(ERLC) $(ERLC_FLAGS) $(EUNIT_ERLC_FLAGS) -pa $(ebin) -o $(test) $<

$(src)/%.erl: $(src)/%.yrl
	$(ERLC) -o $(src) $<

$(src)/%.erl: $(src)/%.xrl
	$(ERLC) -o $(src) $<
	build/prepend_edoc_autogenerated "$@" "$<"

$(ebin):
	mkdir -pv $(ebin)

##
## Some extra dependencies, not covered by default rules above
##

# To compile gpb_compile, we first need the parse transform in gpb_codegen
$(ebin)/gpb_compile.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_decoders_lib.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_gen_encoders.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_gen_decoders.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_gen_mergers.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_gen_verifiers.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_gen_introspect.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_gen_translators.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_gen_json_encoders.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_gen_json_decoders.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_gen_nif.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_lib.beam: $(ebin)/gpb_codegen.beam
$(ebin)/gpb_codemorpher.beam: $(ebin)/gpb_codegen.beam

$(ebin)/gpb.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_analyzer.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_analyzer.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_codemorpher.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_compile.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_compile.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_compile.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_decoders_lib.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_decoders_lib.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_decoders_lib.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_decoders_lib.beam: $(src)/gpb_decoders_lib.hrl
$(ebin)/gpb_defs.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_decoders.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_decoders.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_gen_decoders.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_decoders.beam: $(src)/gpb_decoders_lib.hrl
$(ebin)/gpb_gen_encoders.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_encoders.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_gen_encoders.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_introspect.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_introspect.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_gen_introspect.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_mergers.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_mergers.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_gen_mergers.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_nif.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_nif.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_gen_nif.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_translators.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_translators.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_gen_translators.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_types.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_types.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_verifiers.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_verifiers.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_gen_verifiers.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_json_encoders.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_json_encoders.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_gen_json_encoders.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_json_decoders.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_gen_json_decoders.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_gen_json_decoders.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_gen_json_decoders.beam: $(src)/gpb_decoders_lib.hrl
$(ebin)/gpb_lib.beam: $(src)/gpb_codegen.hrl
$(ebin)/gpb_lib.beam: $(src)/gpb_compile.hrl
$(ebin)/gpb_lib.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_names.beam: $(incdir)/gpb.hrl
$(ebin)/gpb_parse.beam: $(incdir)/gpb.hrl

# To compile gpb_codegen_tests, we first need the parse transform in gpb_codegen
$(test)/gpb_codegen_tests.beam: $(ebin)/gpb_codegen.beam

# To compile gpb.erl, we need gpb_include.hrl
$(ebin)/gpb.beam: $(src)/gpb.erl $(incdir)/gpb_version.hrl

# gpb_compile_tests.erl includes gpb_tests.erl (see the files for details
# on this unorthodox setup), this dependency needs to be recorded
$(test)/gpb_compile_tests.beam: $(test)/gpb_compile_tests.erl \
				$(test)/gpb_tests.erl

# To compile the description generator (and its unit tests), we
# must first have compiled the proto file for the gpb_description.proto
$(ebin)/gpb_compile_descr.beam: $(descr_src)/gpb_compile_descr.erl \
				$(DESCR_PROTO_ERL)

$(test)/gpb_compile_descr_tests.beam: $(descr_src)/gpb_compile_descr_tests.erl \
				$(DESCR_PROTO_ERL)


$(descr_src)/gpb_%.erl: $(priv)/proto3/google/protobuf/%.proto $(BEAMS)
	@echo Proto-compiling the description definition...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -pa $(ebin) \
		-I $(abspath $(priv)/proto3/google/protobuf) \
		-modprefix gpb_ \
		-o $(descr_src) \
		-s gpb_compile c $(abspath $(<))

# To generate the ebin/gpb.app file, process the src/gpb.app.src file
$(ebin)/gpb.app: $(src)/gpb.app.src | $(ebin)
	@echo Generating $@...
	$(silencer)$(ERL) +B -noshell -noinput -eval " \
	    try \
		{ok, [{application,App,KVs1}]} = file:consult(\"$<\"), \
		Vsn2 = case lists:keyfind(vsn,1,KVs1) of \
			   {vsn,{cmd,Cmd}} -> \
				string:strip(os:cmd(Cmd),right,$$\n); \
			   {vsn,Vsn1} -> \
				Vsn1 \
		       end, \
		KVs2 = lists:keystore(vsn,1,KVs1,{vsn,Vsn2}), \
		AppTerm  = {application,App,KVs2}, \
		ok = file:write_file( \
		       \"$@\", \
		       iolist_to_binary( \
		         io_lib:format(\"~p.~n\", [AppTerm]))), \
		halt(0) \
	    catch Class:Reason -> \
		ST = erlang:get_stacktrace(), \
		io:format(\"ERROR: {~p,~p~n\" \
			  \"        ~p}~n\", [Class,Reason,ST]), \
		halt(1) \
	    end."

$(incdir)/gpb_version.hrl: $(incdir)/gpb_version.hrl.in
	@echo Generating $@...
	$(silencer)$(build)/mk_version_hrl \
	    < $(incdir)/gpb_version.hrl.in \
	    > $(incdir)/gpb_version.hrl
