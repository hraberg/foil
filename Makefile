PROJECT = $(shell head project.clj -n1 | awk '{ print $$2 }' )
VERSION = $(shell head project.clj -n1 | awk '{ print $$3 }' | sed s/\"//g )
TARGET = ./target

CXXFLAGS ?= -std=c++14 -pedantic-errors -Wall -Wextra -Werror -Wconversion -O2 -I$(TARGET)
SANITIZE_FLAGS ?= -fsanitize=address

UBERJAR = $(TARGET)/$(PROJECT)-$(VERSION)-standalone.jar
NATIVE_IMAGE = $(TARGET)/foilc

FOIL_CORE = $(TARGET)/foil/core.hpp
TESTS = $(shell find test/foil -name *_test\.cljc | sed s/^test.foil.// | sed s/\.cljc$$// )

.PHONY: all clean check native-image

.PRECIOUS: $(TARGET)/%.cpp $(TARGET)/%.hpp $(TARGET)/%.hpp.ghc

all: $(UBERJAR)

$(UBERJAR): project.clj src/clj/*
	lein uberjar

clean:
	rm -rf $(TARGET)

$(TARGET)/%.cpp: test/foil/%.cljc $(UBERJAR)
	java -jar $(UBERJAR) $< $@

$(TARGET)/%.hpp: src/foil/%.cljc $(UBERJAR)
	java -jar $(UBERJAR) $< $@

$(TARGET)/%.hpp.ghc: $(TARGET)/%.hpp
	$(CXX) $< $(CPPFLAGS) $(CXXFLAGS) -o $@

$(TARGET)/%.s: $(TARGET)/%.cpp $(FOIL_CORE)
	$(CXX) $< $(CPPFLAGS) $(CXXFLAGS) -fno-exceptions -fno-asynchronous-unwind-tables -fno-rtti -S -o- | c++filt > $@

$(TARGET)/%.lst: $(TARGET)/%.cpp $(FOIL_CORE)
	$(CXX) $< $(CPPFLAGS) $(CXXFLAGS) -fno-exceptions -fno-asynchronous-unwind-tables -fno-rtti -g -c -Wa,-adhln -o /dev/null | c++filt > $@

$(TARGET)/%: $(TARGET)/%.cpp $(FOIL_CORE).ghc
	$(CXX) $< $(CPPFLAGS) $(CXXFLAGS) $(SANITIZE_FLAGS) -o $@

check: $(foreach TEST, $(TESTS), $(TARGET)/$(TEST))
	@for TEST in $^; do ./$$TEST; done

$(NATIVE_IMAGE): $(UBERJAR)
	$(GRAAL_HOME)/bin/native-image \
		--no-server \
		-H:+ReportExceptionStackTraces \
		--report-unsupported-elements-at-runtime \
		-H:IncludeResources='.*/?.*cljc$' \
		-jar $(UBERJAR) $(NATIVE_IMAGE)

native-image: $(NATIVE_IMAGE)
