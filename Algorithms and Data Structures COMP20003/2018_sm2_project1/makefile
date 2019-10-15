dict1: stage1.o common.o
	gcc -o dict1 stage1.o common.o

stage1.o: stage1.c header.h
	gcc -c stage1.c

dict2: stage2.o common.o
	gcc -o dict2 stage2.o common.o

stage2.o: stage2.c header.h
	gcc -c stage2.c

common.o: common.c header.h
	gcc -c common.c
