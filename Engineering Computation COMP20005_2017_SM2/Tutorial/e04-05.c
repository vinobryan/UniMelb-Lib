/* Exercise 4.5: Simple character graph
   Alistair Moffat, March 2013
*/

#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char *argv[]) {

	int nstars, i;

	/* initial prompt */
	printf("Enter numbers: ");

	/* now process the sequence of numbers one at a time */
	while (scanf("%d", &nstars) == 1) {
		/* print the value that was read */
		printf("%2d |", nstars);
		/* print the indicated number of stars */
		for (i=0; i<nstars; i++) {
			printf("*");
		}
		/* end of one line */
		printf("\n");
	}

	/* A few more comments:
		-- negative numbers are treated as zero
		-- big numbers result in long lines
		-- processing stops as soon as the scanf guard in
		   the loop returns 0
		-- and when it does, the program ends
	*/
	return 0;
}
