/* Exercise 4.9: Compute the next prime number
   Alistair Moffat, March 2013
*/

#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char *argv[]) {

	int n, prime_found, factor_found, d;

	/* read a starting point */
	printf("Enter an integer value: ");
	if (scanf("%d", &n) != 1) {
		printf("Invalid input\n");
		exit(EXIT_FAILURE);
	}

	/* now loop, trying out consecutive values */
	prime_found = 0;
	while (!prime_found) {
		/* step n to next number... */
		n += 1; 

		/* ... and test to see if it is prime */
		factor_found = 0;
		for (d=2; d*d<=n; d++) {
			if (n%d == 0) {
				/* n isn't prime, so can stop loop */
				factor_found = 1;
				break;
			}
		}

		/* if no factors are found, n is prime */
		prime_found = (!factor_found);
	}

	/* will only get to this point if a number with no factors
	   has been identified, which means it is prime... */
	printf("The next prime is     : %d\n", n);

	/* job done */
	return 0;
}
