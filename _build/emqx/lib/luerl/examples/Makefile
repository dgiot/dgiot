SUBDIRS ?= hello

all clean:
	@for subdir in $(SUBDIRS); do					\
	  echo $(MAKE) -C $$subdir $@;					\
	  $(MAKE) -C $$subdir $@;					\
	done

.PHONY: all clean
