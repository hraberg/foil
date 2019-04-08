PROJECT = $(shell head project.clj -n1 | awk '{ print $$2 }' )
VERSION = $(shell head project.clj -n1 | awk '{ print $$3 }' | sed s/\"//g )
TARGET = ./target

CXXFLAGS ?= -std=c++14 -pedantic-errors -Wall -Wextra -Werror -Wconversion -O2 -I$(TARGET)

UBERJAR = $(TARGET)/$(PROJECT)-$(VERSION)-standalone.jar
NATIVE_IMAGE = $(TARGET)/foilc

FOIL_CORE = $(TARGET)/foil/core.hpp

.PHONY: all clean check native-image

.PRECIOUS: $(TARGET)/%.cpp $(TARGET)/%.hpp $(TARGET)/%.hpp.ghc

all: $(UBERJAR)

$(UBERJAR): src/clj/*
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
	$(CXX) $< $(CPPFLAGS) $(CXXFLAGS) -fsanitize=address -o $@

check: $(TARGET)/foil/core_test
	$<

$(NATIVE_IMAGE): $(UBERJAR)
	$(GRAAL_HOME)/bin/native-image --no-server -H:+ReportExceptionStackTraces --report-unsupported-elements-at-runtime -jar $(UBERJAR) $(NATIVE_IMAGE)

native-image: $(NATIVE_IMAGE)
