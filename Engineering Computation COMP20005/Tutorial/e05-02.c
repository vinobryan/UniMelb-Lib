/* Exercise 5.2: Find the largest among four integers
   Jianzhong Qi, March 2013.
*/

#include <stdio.h>
#include <stdlib.h>

int max_2_ints(int n1, int n2);
int max_4_ints(int n1, int n2, int n3, int n4);

int
main(int argc, char *argv[]) {

	int n1, n2, n3, n4, max;												
	/* get four integers */
	printf("Enter four integers: ");
	if (scanf("%d %d %d %d", &n1, &n2, &n3, &n4) != 4) {
		printf("Error in input\n");
		exit(EXIT_FAILURE);
	}

	/* scaffolding to test the function */
	max = max_4_ints(n1, n2, n3, n4);
	printf("max_4_ints(%d, %d, %d, %d) = %d\n", n1, n2, n3, n4, max);
	
	/* job done */
	return 0;
}

/* return the larger of two integers n1 and n2 */
int 
max_2_ints(int n1, int n2) {
	if (n1>n2) {
		return n1;
	} else {
		return n2;
	}
}

/* return the largest or four integers n1, n2, n3 and n4 */
int 
max_4_ints(int n1, int n2, int n3, int n4) {
	int max;
	max = n1;
	max = max_2_ints(max, n2);
	max = max_2_ints(max, n3);
	max = max_2_ints(max, n4);
	return max;
	/* or just
	   return max_2_ints(max_2_ints(n1, n2), max_2_ints(n3, n4));
	*/
}
