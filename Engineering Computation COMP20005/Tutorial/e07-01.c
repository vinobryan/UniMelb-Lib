/* Exercise 7.1: Check if an int array contains non-zero integers
   Jianzhong Qi, May 2013
   Note: read_int_array() and print_int_array() are from the textbook.
 */

#include <stdio.h>

/* maximum array size allowed */
#define MAXVALS 1000

/* function prototypes */
int read_int_array(int A[], int n);
int all_zero(int A[], int n);
void print_int_array(int A[], int n);

int
main(int argc, char *argv[]) {

	int numbers[MAXVALS], valid;   

	/* get the input */
	valid = read_int_array(numbers, MAXVALS);
	printf("The array: ");
	print_int_array(numbers, valid);
    
	/* check if the array contains non-zero integers */
	if (all_zero(numbers, valid)) {
		printf("only contains zeros\n");
	} else {
		printf("contains non-zero values\n");
	}

	/* done */
	return 0;
}

/* check if the array contains non-zero integers */
int
all_zero(int A[], int n) {
	int i;
	/* check the items one by one */
	for (i=0; i<n; i++) {
		if (A[i]) {
			return 0;
		}
	}
	/* got all the way through, no non-zeros found */
	return 1;
}


/* read integers from keyboard and store them in an array. */
int
read_int_array(int A[], int maxvals) {
	int n=0, excess=0, next;

	printf("Enter as many as %d values, ^D to end\n", maxvals);
	while (scanf("%d", &next) == 1) {
		if (n == maxvals) {
			excess = excess + 1;
		} else {
			A[n] = next;
			n += 1;
		}
	}

	printf("%d values read into array", n);
	if (excess) {
		printf(", %d excess values discarded", excess);
	}
	printf("\n");
	return n;
}


/* print the elements of an array */
void
print_int_array(int A[], int n) {
	int i;

	for (i=0; i<n; i++) {
		printf("%d ", A[i]);
	}
	putchar('\n');
	return;
}
