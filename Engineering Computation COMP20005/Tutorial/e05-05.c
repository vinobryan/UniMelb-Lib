/* Exercise 5.5: Finding perfect numbers
   Alistair Moffat, March 2013.
*/

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>


/* function prototypes */
int isperfect(int n);
int nextperfect(int n);
int sumfactors(int n);

int
main(int argc, char *argv[]) {
	int n;

	/* scaffolding to fetch an input value */
	printf("Enter a number n: ");
	if (scanf("%d", &n) != 1) {
		printf("Invalid input\n");
		exit(EXIT_FAILURE);
	}

	/* and try it out in the two functions */
	if (isperfect(n)) {
		printf("%d is a perfect number\n", n);
	} else {
		printf("%d is not a perfect number\n", n);
	}
	printf("The next perfect is : %d\n", nextperfect(n));
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

/* determine whether n is perfect */
int
isperfect(int n) {
	/* yes, it is perfectly (boom boom) ok for a function to just
	   have one line */
	return (sumfactors(n) == n);
}

/* determine the next perfect number after n */
int
nextperfect(int n) {
	n = n+1;
	/* and just search until one is found */
	while (!isperfect(n)) {
		n = n+1;
		/* time to be a bit careful... */
		if (n==INT_MAX) {
			/* ...and time to bail out */
			printf("No more perfect numbers\n");
			exit(EXIT_FAILURE);
		}
	}
	/* got one */
	return n;
}
