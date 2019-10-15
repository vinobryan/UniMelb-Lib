/* Exercise 7.6 in Programming, Problem Solving, and Abstraction

   Read an a set of numbers, sort using selection sort.
   Two versions are given, one recursive. Could also implement
   function index_of_largest using recursion if really want to.
   But no real advantage in this case, a loop is also fine.

   Function bubblesort is replaced in the program shown in
   Figures 7.2, 7.3, and 7.4

   Alistair Moffat, August 2012

*/

#include <stdio.h>

#define MAXVALS 1000

/* function prototypes */

int read_int_array(int[], int);
int index_of_largest(int[], int);
void sort_int_array(int[], int);
void sort_int_array_rec(int[], int);
void print_int_array(int[], int);
void int_swap(int*, int*);

int
main(int argc, char **argv) {
	int numbers[MAXVALS], valid;
	valid = read_int_array(numbers, MAXVALS);
	printf("Before: ");
	print_int_array(numbers, valid);
	sort_int_array(numbers, valid);
	printf("After1: ");
	print_int_array(numbers, valid);
	sort_int_array_rec(numbers, valid);
	printf("After2: ");
	print_int_array(numbers, valid);
	return 0;
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

/* a new helper function  -- returns index of largest value
   in the suppied array
*/
int
index_of_largest(int A[], int n) {
	int big, i;
	big = 0;
	for (i=1; i<n; i++) {
		if (A[i]>A[big]) {
			big = i;
		}
	}
	return big;
}

/* the new function -- implements selection sort by repeatedly swapping
   the next largest item in the array into the last position of a
   decreasing array segment
*/
void
sort_int_array(int A[], int n) {
	int i, big;
	for (i=n; i>0; i--) {
		/* find largest val in A[0..i-1] */
		big = index_of_largest(A, i);
		/* and swap it to the last location */
		int_swap(A+big, A+i-1);
	}
	return;
}

/* and this one does exactly the same task, but recursively
*/
void
sort_int_array_rec(int A[], int n) {
	int big;
	if (n<=1) {
		/* easy cases, already sorted */
		return;
	}
	big = index_of_largest(A, n);
	int_swap(A+big, A+n-1);
	/* now get a friend to do the rest of the work */
	sort_int_array_rec(A, n-1);
	return;
}

void
print_int_array(int A[], int n) {
	int i;
	for (i=0; i<n; i++) {
		printf("%3d", A[i]);
	}
	printf("\n");
}

/* exchange the values of the two variables indicated 
	by the arguments */
void
int_swap(int *x, int *y) {
	int t;
	t = *x;
	*x = *y;
	*y = t;
}
