/* Exercise 7.2: Sort an array into decreasing order.
   Just a one-character change from the insertionsort function that
   is presented in Figure 7.3, can you see it?
   Alistair Moffat, May 2013.
*/
#include <stdio.h>

#define MAXVALS 1000

int read_int_array(int A[], int n);
void sort_int_array(int A[], int n);
void print_int_array(int A[], int n);
void int_swap(int *p1, int *p2);

int
main(int argc, char *argv[]) {
	int numbers[MAXVALS], nnumbs;
	nnumbs = read_int_array(numbers, MAXVALS);
	printf("Before: ");
	print_int_array(numbers, nnumbs);
	sort_int_array(numbers, nnumbs);
	printf("After : ");
	print_int_array(numbers, nnumbs);
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

void
sort_int_array(int A[], int n) {
	int i, j;
	/* assume that A[0] to A[n-1] have valid values */
	for (i=1; i<n; i++) {
		/* swap A[i] left into correct position, where
		   "correct" is defined by *decreasing* order */
		for (j=i-1; j>=0 && A[j+1]>A[j]; j--) {
				       /* ^ the critical change */
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
