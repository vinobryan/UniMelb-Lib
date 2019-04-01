/* Exercise 5.3: Compute an integer power
   Jianzhong Qi, March 2013.
*/

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>

int int_pow(int base, int exponent);

int
main(int argc, char *argv[]) {

	int base, exponent, power;												
	/* get two integers */
	printf("Enter the base and the exponent: ");
	if ((scanf("%d %d", &base, &exponent)!=2) || (exponent<=0)) {
		printf("Error in input\n");
		exit(EXIT_FAILURE);
	}

	/* scaffolding to call the function */
	power = int_pow(base, exponent);
	printf("%d^%d = %d\n", base, exponent, power);
	
	/* job done */
	return 0;
}


/* compute and return 'base' to the power of 'exponent' */
int 
int_pow(int base, int exponent) {
	
	int i;
	int power=1;
		
	for (i=0; i<exponent; i++) {
		/* beware of overflow, have to test via division */
		if ((base!=0) && (abs(INT_MAX/base)<abs(power))) { 
			printf("Integer overflow in int_pow\n");
			exit(EXIT_FAILURE);
		}
		/* now safe to proceed */
		power *= base;
	}
	return power;
}
