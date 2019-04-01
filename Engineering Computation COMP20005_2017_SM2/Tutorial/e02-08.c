/* Exercise 2.8: Convert Fahrenheit to Celsius
   Alistair Moffat, March 2013.
*/

#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char *argv[]) {

	double degC, degF;

	/* get the input value */
	printf("Enter degrees F: ");
	if (scanf("%lf", &degF) != 1) {
		/* the if statement is introduced in Chapter 3 */
		printf("Error in input\n");
		exit(EXIT_FAILURE);
	}

	/* do the conversion */
	degC = (degF-32.0)*(5.0/9.0);
	/* in a more sophisticated program that expression would be
	   in a function (Chapter 5). In this specific case, because 
	   it is clear what is happening, I'm willing to tolerate
	   embedded constants rather than use of #define */

	/* print the equivalent output */
	printf("In degrees C is: %.1f\n", degC);

	return 0;
}
