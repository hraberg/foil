PROJECT = $(shell head project.clj -n1 | awk '{ print $$2 }' )
VERSION = $(shell head project.clj -n1 | awk '{ print $$3 }' | sed s/\"//g )
TARGET = ./target

UBERJAR = $(TARGET)/$(PROJECT)-$(VERSION)-standalone.jar

.PHONY: all uberjar clean run

all: $(UBERJAR)

$(UBERJAR): src/*
	lein uberjar

clean:
	rm -rf $(TARGET)

run: $(UBERJAR)
	java -jar $(UBERJAR)
