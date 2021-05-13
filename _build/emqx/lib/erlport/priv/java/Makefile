JAR_PKG = _pkgs/erlport.jar

# Source files
SOURCE_FILES = \
erlport/terms/Tag.java \
erlport/terms/Atom.java \
erlport/terms/Binary.java \
erlport/terms/Pid.java \
erlport/terms/Port.java \
erlport/terms/Reference.java \
erlport/terms/Tuple.java \
erlport/Utils.java \
erlport/Request.java \
erlport/Response.java \
erlport/Port.java \
erlport/CLI.java \

JAVAC = javac

JFLAGS = -encoding UTF-8# -Xlint:unchecked

#### Compile settings ####

.PHONY: default build new clean run jar

vpath %.class classes
vpath %.java src

$(JAR_PKG):
ifeq (,$(wildcard $(JAR_PKG)))
	@$(MAKE) build
	@jar cf $(JAR_PKG) -C classes .
	@rm -rf classes
	@echo "Successfully packing erlport.jar"
endif

build: ensure_dirs $(SOURCE_FILES:.java=.class)

rebuild: clean $(JAR_PKG)

ensure_dirs:
	@mkdir -p _pkgs
	@mkdir -p classes

%.class: %.java
	$(JAVAC) -cp classes -d classes $(JFLAGS) $<

clean:
	@rm -rf classes/*
	@rm -rf $(JAR_PKG)
