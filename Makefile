PROJECT = $(shell head project.clj -n1 | awk '{ print $$2 }' )
VERSION = $(shell head project.clj -n1 | awk '{ print $$3 }' | sed s/\"//g )
TARGET = ./target

CXXFLAGS = -std=c++14 -pedantic-errors -Wall -Wextra -Werror -Wconversion -O2 -I$(TARGET)

UBERJAR = $(TARGET)/$(PROJECT)-$(VERSION)-standalone.jar
NATIVE_IMAGE=$(TARGET)/foilc

.PHONY: all uberjar clean run check native-image

all: $(UBERJAR)

$(UBERJAR): src/clj/*
	lein uberjar

clean:
	rm -rf $(TARGET)

$(TARGET)/%.cpp: test/foil/%.cljc $(UBERJAR)
	java -jar $(UBERJAR) $< $@

$(TARGET)/foil/%.hpp: src/foil/foil/%.cljc $(UBERJAR)
	mkdir -p `dirname $@`
	java -jar $(UBERJAR) $< $@

$(TARGET)/foil/%.hpp.ghc: $(TARGET)/foil/%.hpp
	$(CXX) $< $(CXXFLAGS) -o $@

$(TARGET)/%.s: $(TARGET)/%.cpp $(TARGET)/foil/core.hpp
	$(CXX) $< $(CXXFLAGS) -fno-exceptions -fno-asynchronous-unwind-tables -fno-rtti -S -o- | c++filt > $@

$(TARGET)/%.lst: $(TARGET)/%.cpp $(TARGET)/foil/core.hpp
	$(CXX) $< $(CXXFLAGS) -fno-exceptions -fno-asynchronous-unwind-tables -fno-rtti -g -c -Wa,-adhln -o /dev/null | c++filt > $@

$(TARGET)/%: $(TARGET)/%.cpp $(TARGET)/foil/core.hpp.ghc
	$(CXX) $< $(CXXFLAGS) -o $@

check: $(TARGET)/example $(TARGET)/example.cpp $(TARGET)/foil/core.hpp $(TARGET)/foil/core.hpp.ghc
	$< | (diff -u test/foil/example.out - && echo "Tests PASSED")

$(NATIVE_IMAGE): $(UBERJAR)
	$(GRAAL_HOME)/bin/native-image --no-server -H:+ReportExceptionStackTraces --report-unsupported-elements-at-runtime -jar $(UBERJAR) $(NATIVE_IMAGE)

native-image: $(NATIVE_IMAGE)

run: $(UBERJAR)
	java -jar $(UBERJAR)
