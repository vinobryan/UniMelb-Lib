/* Exercise 6.6 of Programming, Problem Solving, and Abstraction
   Alistair Moffat, August 2012
*/

#include <stdio.h>
#include <stdlib.h>

/* previous function: test two integers and exchange if out of order */
void
int_sort2(int *x1, int *x2) {
	int t;
	if (*x1 > *x2) {
		/* need to do the swap */
		t = *x1;
		*x1 = *x2;
		*x2 = t;
	}
	return;
}

/* required function: test three integers and exchange if out of order */
void
int_sort3(int *x1, int *x2, int *x3) {
	/* allowed to make use of int_sort2(), makes life much easier;
	   note that the arguments are already pointers so can be passed
	   through unchanged to next function */
	int_sort2(x1, x2);
	int_sort2(x2, x3);
	int_sort2(x1, x2);
	/* the only hard part in there is to get the pattern of arguments
	   right */
	return;
}


/* scaffolding to test the required function */
int
main(int argc, char **argv) {
	int v1, v2, v3;

	/* get three values */
	printf("Enter three integers: ");
	if (scanf("%d%d%d", &v1, &v2, &v3) != 3) {
		printf("Invalid input\n");
		exit(EXIT_FAILURE);
	}

	/* and then test-call the function with them */
	printf("Before: v1 = %d, v2 = %d, v3 = %d\n", v1, v2, v3);
	int_sort3(&v1, &v2, &v3);
	printf("After:  v1 = %d, v2 = %d, v3 = %d\n", v1, v2, v3);

	/* no worries */
	return 0;
}
