CC=gcc
CPPFLAGS=-Wall  -Werror  -O3 -lm
#CPPFLAGS=-Wall  -Werror  -g

SRC=src/utils.o src/priority_queue.o src/ai.o src/2048.o 
TARGET=2048

all: $(SRC)
	$(CC) -o $(TARGET) $(SRC) $(CPPFLAGS)

clean:
	rm -f $(TARGET) src/*.o
