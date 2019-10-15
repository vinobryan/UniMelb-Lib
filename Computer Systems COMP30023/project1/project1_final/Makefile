# 
# Makefile for project 1 
#
# Ziren Xiao ID: 675485
#


## CC  = Compiler.
## CFLAGS = Compiler flags.
CC	= gcc
CFLAGS =	-Wall -Wextra -std=gnu99


## OBJ = Object files.
## SRC = Source files.
## EXE = Executable name.

SRC =		swap.c scheduler.c input.c main.c memory.c output.c queue.c
OBJ =		swap.o scheduler.o input.o main.o memory.o output.o queue.o
EXE = 		swap

## Top level target is executable.
$(EXE):	$(OBJ)
		$(CC) $(CFLAGS) -o $(EXE) $(OBJ) -lm


## Clean: Remove object files and core dump files.
clean:
		/bin/rm $(OBJ) 

## Clobber: Performs Clean and removes executable file.

clobber: clean
		/bin/rm $(EXE) 

## Dependencies

swap.o:	header.h
scheduler.o:	header.h
input.o:	header.h
main.o:	header.h
memory.o:	header.h
output.o:	header.h
queue.o:	header.h

