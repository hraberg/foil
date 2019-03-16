PROJECT = $(shell head project.clj -n1 | awk '{ print $$2 }' )
VERSION = $(shell head project.clj -n1 | awk '{ print $$3 }' | sed s/\"//g )
TARGET = ./target

CXXFLAGS = -std=c++17 -pedantic-errors -Wall -Wextra -Werror -Wconversion -O2

UBERJAR = $(TARGET)/$(PROJECT)-$(VERSION)-standalone.jar
NATIVE_IMAGE=$(TARGET)/foil

.PHONY: all uberjar clean run check native-image

all: $(UBERJAR)

$(UBERJAR): src/*
	lein uberjar

clean:
	rm -rf $(TARGET)

$(TARGET)/%.cc: test/foil/%.clj $(UBERJAR)
	cat $< | java -jar $(UBERJAR) > $@

$(TARGET)/%.s: $(TARGET)/%.cc
	$(CXX) $< $(CXXFLAGS) -S -o $@

$(TARGET)/%: $(TARGET)/%.cc
	$(CXX) $< $(CXXFLAGS) -o $@

check: $(TARGET)/example $(TARGET)/example.s $(TARGET)/example.cc
	$< | (diff -u test/foil/example.out - && echo "Tests PASSED")

$(NATIVE_IMAGE): $(UBERJAR)
	$(GRAAL_HOME)/bin/native-image --no-server -H:+ReportExceptionStackTraces --report-unsupported-elements-at-runtime -jar $(UBERJAR) $(NATIVE_IMAGE)

native-image: $(NATIVE_IMAGE)

run: $(UBERJAR)
	java -jar $(UBERJAR)
