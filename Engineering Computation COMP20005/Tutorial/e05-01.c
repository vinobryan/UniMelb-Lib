/* Exercise 5.1: Find the larger of two integers
   Jianzhong Qi, March 2013.
*/

#include <stdio.h>
#include <stdlib.h>

int max_2_ints(int n1, int n2);

int
main(int argc, char *argv[]) {

	int n1, n2, max;												
	/* get two integers */
	printf("Enter two integers: ");
	if (scanf("%d %d", &n1, &n2) != 2) {
		printf("Error in input\n");
		exit(EXIT_FAILURE);
	}

	/* scaffolding -- get the larger integer */
	max = max_2_ints(n1, n2);
	printf("max_2_ints(%d, %d) = %d\n", n1, n2, max);
	
	/* job done */
	return 0;
}

/* return the larger between two integers n1 and n2 */
int 
max_2_ints(int n1, int n2) {
	if (n1>n2) {
		return n1;
	} else {
		return n2;
	}
}
