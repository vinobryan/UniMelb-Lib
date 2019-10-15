/* Exercise 3.7: Convert Fahrenheit to Celsius and also vice versa
   Alistair Moffat, March 2013.
*/

#include <stdio.h>
#include <stdlib.h>

#define DEGREES_C 'C'
#define DEGREES_F 'F'

#define TEMP_CONS_1 32.0
#define TEMP_CONS_2  5.0
#define TEMP_CONS_3  9.0

int
main(int argc, char *argv[]) {

	double degC, degF, deg;
	char c_or_f;

	/* get the input value and units code*/
	printf("Enter a temperature: ");
	if (scanf("%lf%c", &deg, &c_or_f) != 2) {
		printf("Error in input\n");
		exit(EXIT_FAILURE);
	}

	/* check the units character that was read */
	if (c_or_f == DEGREES_C) {
		/* Celsius to Fahrenheit conversion required */
		degC = deg;
		/* would be much better to have a function for this */
		degF = degC*(TEMP_CONS_3/TEMP_CONS_2) + TEMP_CONS_1;
		printf("The temperature %.1f%c converts to %.1f%c\n",
			degC, DEGREES_C, degF, DEGREES_F);
	} else if (c_or_f == DEGREES_F) {
		/* Fahrenheit to Celsius conversion required */
		degF = deg;
		/* and another function for the reverse computation */
		degC = (degF-TEMP_CONS_1)*(TEMP_CONS_2/TEMP_CONS_3);
		printf("The temperature %.1f%c converts to %.1f%c\n",
			degF, DEGREES_F, degC, DEGREES_C);
	} else {
		printf("Invalid units, need %c or %c\n", DEGREES_C, DEGREES_F);
		exit(EXIT_FAILURE);
	}

	/* job done */
	return 0;
}
