CC=gcc
#CPPFLAGS=-Wall   -O3 -std=gnu99
CPPFLAGS=   -g  -std=gnu99

SRC=src/utils.o src/priority_queue.o src/ai.o src/pacman.o 
TARGET=pacman

all: $(SRC)
	$(CC) -o $(TARGET) $(SRC) $(CPPFLAGS) -lncurses -lm

clean:
	rm -f $(TARGET) src/*.o

