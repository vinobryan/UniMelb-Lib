yelp1: main.o makedict.o insert.o search.o construct.o
	gcc -o yelp1 main.o makedict.o insert.o search.o construct.o

main.o: main.c header.h
	gcc -c main.c

makedict.o: makedict.c header.h
	gcc -c makedict.c

insert.o: insert.c header.h
	gcc -c insert.c

search.o: search.c header.h
	gcc -c search.c

construct.o: construct.c header.h
	gcc -c construct.c

clean:
	rm -rf yelp1
	rm -rf *.o
