/* Exercise 7.4: Compute the frequencies of the integers in an array.
   Based on the insertionsort.c program shown in Chapter 7.
   Jianzhong Qi and Alistair Moffat, May 2013
 */

#include <stdio.h>
#include <limits.h>
#include <stdlib.h>

/* maximum array size allowed */
#define MAXVALS 1000

int read_int_array(int A[], int n);
void sort_int_array(int A[], int n);
void print_int_array(int A[], int n);
void int_swap(int *p1, int *p2);
void print_freqs_sorting(int A[], int n);
void print_freqs_2ndarray(int A[], int n);

int
main(int argc, char *argv[]) {
	int numbers[MAXVALS], nnumbs;
	nnumbs = read_int_array(numbers, MAXVALS);
	print_freqs_sorting(numbers, nnumbs);
	print_freqs_2ndarray(numbers, nnumbs);
	return 0;
}

/* compute the frequencies of the integers in an array 
   by first sorting the array and then counting the frequencies of
   items as they appear in the sorted list
*/
void 
print_freqs_sorting(int A[], int n) {
	int i, current, count;
	/* defensive test to allow the rest of the function to be 
	   simpler */
	if (n==0) {
		return;
	}

	/* get the elements into order */
	sort_int_array(A, n);
	
	printf("Value\tFreq\n");
	/* use the first item in the array to prime the pump */
	current = A[0];
	count = 1;
	for (i=1; i<n; i++) {
		if (A[i]==current) {
			/* member of the current group being built up */
			count++;
		} else {
			/* finished with the previous group */
			printf("%4d\t%3d\n", current, count);
			/* an dthen start a new group */
			current = A[i];
			count = 1;
		}
	}
	/* don't forget the last group */
	printf("%4d\t%3d\n", current, count);
	return;
}

#define BIGGEST 1000
#define SMLLEST -1000

/* compute the frequencies of the integers in an array 
   using a second array indexed by the values. Might not always
   be possible, needs to check the input carefully to make sure
   all values are in the range that has been guessed for the second
   array.
*/
void 
print_freqs_2ndarray(int A[], int n) {
	/* create a counter for every possible integer, need to guess
	   the range of values and then report if cannot be done 
	 */
	int frequency[BIGGEST-SMLLEST+1];
	int i, mapped_val;
	
	/* initialise all counters to zero */
	for (i=0; i<BIGGEST-SMLLEST+1; i++) {
		frequency[i] = 0;
	}
	
	/* process the original array, mapping A's values into
	   positions in the range [SMLLEST..BIGGEST] */
	for (i=0; i<n; i++) {
		mapped_val = A[i] - SMLLEST;
		/* and test carefully! */
		if (mapped_val<0 || mapped_val>=(BIGGEST-SMLLEST+1)) {
			printf("Error: Value %d in array is too big, ",
				A[i]);
			printf("must be between %d and %d\n",
				SMLLEST, BIGGEST);
			exit(EXIT_FAILURE);
		}
		/* ok, so mapped value is in the right range */
		frequency[mapped_val] += 1;
	}
	
	/* now output the frequencies */
	printf("Value\tFreq\n");
	for (mapped_val=0; mapped_val<BIGGEST-SMLLEST+1; mapped_val++) {
		/* only output if frequency is larger than 0 */
		if (frequency[mapped_val]) {
			/* remember to unmap the value */
			i = mapped_val + SMLLEST;
			printf("%4d\t%3d\n", i, frequency[mapped_val]);
		}
	}
	return;
}

int
read_int_array(int A[], int maxvals) {
	int n, excess, next;
	printf("Enter as many as %d values, ^D to end\n",
			maxvals);
	n = 0; excess = 0;
	while (scanf("%d", &next)==1) {
		if (n==maxvals) {
			excess = excess+1;
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

void
sort_int_array(int A[], int n) {
	int i, j;
	/* assume that A[0] to A[n-1] have valid values */
	for (i=1; i<n; i++) {
		/* swap A[i] left into correct position */
		for (j=i-1; j>=0 && A[j+1]<A[j]; j--) {
			/* not there yet */
			int_swap(&A[j], &A[j+1]);
		}
	}
	/* and that's all there is to it! */
}

void
print_int_array(int A[], int n) {
	int i;
	for (i=0; i<n; i++) {
		printf("%4d", A[i]);
	}
	printf("\n");
}

/* exchange the values of the two variables indicated 
	by the arguments */
void
int_swap(int *p1, int *p2) {
	int tmp;
	tmp = *p1;
	*p1 = *p2;
	*p2 = tmp;
}
