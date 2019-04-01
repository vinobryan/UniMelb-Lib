yelp1: main.o output.o input.o construct.o
	gcc -o yelp1 main.o output.o input.o construct.o

yelp2: main2.o output2.o input2.o construct2.o
	gcc -o yelp2 main2.o output2.o input2.o construct2.o

main.o: main.c header.h
	gcc -c main.c

output.o: output.c header.h  
	gcc -c output.c

construct.o: construct.c header.h  
	gcc -c construct.c

input.o: input.c header.h  
	gcc -c input.c

main.o2: main2.c header2.h
	gcc -c main2.c

output2.o: output2.c header2.h  
	gcc -c output2.c

construct2.o: construct2.c header2.h  
	gcc -c construct2.c

input2.o: input2.c header2.h  
	gcc -c input2.c
      
clean:  
	rm -rf yelp1
	rm -rf yelp2
	rm -rf *.o
