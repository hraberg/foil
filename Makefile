PROJECT = $(shell head project.clj -n1 | awk '{ print $$2 }' )
VERSION = $(shell head project.clj -n1 | awk '{ print $$3 }' | sed s/\"//g )
TARGET = ./target

CFLAGS = -std=c11 -pedantic-errors -Wall -Wextra -O

UBERJAR = $(TARGET)/$(PROJECT)-$(VERSION)-standalone.jar

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

run: $(UBERJAR)
	java -jar $(UBERJAR)
