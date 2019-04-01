/* Exercise 4.4: Generate a table of printable ASCII characters.
   Alistair Moffat, March 2013
*/

#include <stdio.h>
#include <stdlib.h>

#define FIRST 32	/* first printable ASCII code */
#define LAST 126	/* last printable ASCII code */
#define PERROW 8	/* number of codes to be printed per row */

int
main(int argc, char *argv[]) {
	int c;

	/* first, lay out the table heading row */
	printf("         ");
	for (c=0; c<PERROW; c++) {
		printf("  +%d", c);
	}
	printf("\n        +");

	/* now the separator row */
	for (c=0; c<PERROW; c++) {
		printf("----");
	}

	/* now the characters generated, one by one */
	for (c=FIRST; c<=LAST; c++) {
		/* but with different treatment every now and then
		   give the impression of two-dimensional output
		*/
		if (c%PERROW==0) {
			/* in the row label, treat c as an integer */
			printf("\n    %3d |", c);
		}
		/* and in this one, treat c as a char */
		printf("   %c", c);
	}

	/* after the last row, one more newline, just for neatness */
	printf("\n");

	/* job done */
	return 0;
}
