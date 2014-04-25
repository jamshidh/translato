
CPPFLAGS += -g $(shell pkg-config --cflags gtk+-3.0)
LOADLIBES += $(shell pkg-config --libs gtk+-3.0)

test: flowbox.o break.o

abcd: flowbox.o break.o

program: flowbox.o

clean:
	rm *.o
	rm program
