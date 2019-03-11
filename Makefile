PROJECT = $(shell head project.clj -n1 | awk '{ print $$2 }' )
VERSION = $(shell head project.clj -n1 | awk '{ print $$3 }' | sed s/\"//g )
TARGET = ./target

CFLAGS = -std=c11 -pedantic-errors -Wall -Wextra -O

UBERJAR = $(TARGET)/$(PROJECT)-$(VERSION)-standalone.jar
NATIVE_IMAGE=$(TARGET)/foil

.PHONY: all uberjar clean run check

all: $(UBERJAR)

$(UBERJAR): src/*
	lein uberjar

clean:
	rm -rf $(TARGET)

check: $(UBERJAR)
	cat test/foil/example.clj | java -jar $(UBERJAR) > $(TARGET)/example.c
	gcc $(TARGET)/example.c $(CFLAGS) -o $(TARGET)/example
	$(TARGET)/example | (diff -u test/foil/example.out - && echo "Tests PASSED")

$(NATIVE_IMAGE): $(UBERJAR)
	$(GRAAL_HOME)/bin/native-image --no-server -H:+ReportExceptionStackTraces -jar $(UBERJAR) $(NATIVE_IMAGE)

run: $(UBERJAR)
	java -jar $(UBERJAR)
