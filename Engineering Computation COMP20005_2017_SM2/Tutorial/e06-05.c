/* Exercise 6.5 of Programming, Problem Solving, and Abstraction
   Alistair Moffat, August 2012
*/

#include <stdio.h>
#include <stdlib.h>

/* required function: test two integers and exchange if out of order */
void
int_sort2(int *x1, int *x2) {
	int t;
	if (*x1 > *x2) {
		/* need to do the swap */
		t = *x1;
		*x1 = *x2;
		*x2 = t;
		/* could also have called:
		int_swap(x1, x2);
		to do it, noting that x1 and x2 are already pointers
		*/
	}
	return;
}


/* scaffolding to test that function */
int
main(int argc, char **argv) {
	int v1, v2;

	/* get two numbers */
	printf("Enter two integers: ");
	if (scanf("%d%d", &v1, &v2) != 2) {
		printf("Invalid input\n");
		exit(EXIT_FAILURE);
	}
	printf("Before: v1 = %d, v2 = %d\n", v1, v2);

	/* call the function being tested */
	int_sort2(&v1, &v2);
	printf("After:  v1 = %d, v2 = %d\n", v1, v2);

	return 0;
}
