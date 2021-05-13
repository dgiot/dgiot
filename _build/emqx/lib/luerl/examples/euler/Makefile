EXAMPLES = euler

ROOTDIR = ../..
SRCDIR  = $(ROOTDIR)/src
BEAMDIR = $(ROOTDIR)/ebin

all: $(EXAMPLES)

clean:
	rm -f *.beam erl_crash.dump

.SECONDARY:

%.beam: %.erl $(SRCDIR)/*.hrl
	erlc -I $(SRCDIR) $<

%: %.beam
	erl -pa $(BEAMDIR) -s $@ run -s init stop -noshell

.PHONY: all clean
