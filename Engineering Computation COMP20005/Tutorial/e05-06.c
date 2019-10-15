/* Exercise 5.6: Finding amicable pairs
   Alistair Moffat, March 2013.
*/

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

/* function prototypes */
int amicable_pair(int n1, int n2);
int sumfactors(int n);

int
main(int argc, char *argv[]) {
	int n, sumfactorsn;

	printf("Enter a number n: ");
	if (scanf("%d", &n) != 1) {
		printf("Invalid input\n");
		exit(EXIT_FAILURE);
	}

	/* scaffolding searches from n for first amicable
	   pair where n is one of the partners, then exits loop */
	for (; n<INT_MAX; n++)  {
		/* the only value that n can be amicable with is the
		   sum of its own factors -- and so we may as well reuse
		   a previously written function */
		sumfactorsn = sumfactors(n);
		if (amicable_pair(n, sumfactorsn)) {
			printf("%d and %d are amicable\n",
				n, sumfactorsn);
			break;
		}
	}
	return 0;
}

/* sum the factors of the argument */
int
sumfactors(int n) {
	int i, sum=1;
	/* the sum already includes the first factor */
	for (i=2; i*i<n; i++) {
		/* the loop is much more efficient if stopped at sqrt(n) */
		if (n%i==0) {
			/* found a factor */
			sum += i;
			/* and get two for the price of one */
			sum += (n/i);
		}
	}
	/* one more thing to check, that last value of i */
	if (i*i==n) {
		/* yes, n is a square, and has one more factor */
		sum += i;
	}
	return sum;
}

/* determine whether n1 and n2 form an amicable pair */
int
amicable_pair(int n1, int n2) {
	return (sumfactors(n1)==n2 && sumfactors(n2)==n1 && n1!=n2);
}
