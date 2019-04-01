/*
 * Author: Anthony Sulistio
 * Date: Feb 2003
 * Modified by: Antonette Mendoza and Alistair Moffat
 * Description:
 *      A programming exercise for question 8.2 (page 138).
 *      A program that returns the Euclidean distance between two vectors.
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* define a struct with two members: x and y */
typedef struct {
	double x;
	double y;
} vector_t;

/* function prototype */
double distance(vector_t, vector_t);

int
main(int argc, char *argv[]) {
	vector_t p1, p2;
	double result;

	printf("Enter two points, e.g. (1,2.5) (3,4): ");
	if (scanf("(%lf,%lf) (%lf, %lf)",
			&p1.x, &p1.y, &p2.x, &p2.y) != 4) {
		printf("Invalid input\n");
		exit(EXIT_FAILURE);
	}

	result = distance(p1, p2);
	printf("distance between (%.1f, %.1f) and (%.1f, %.1f) is %.1f\n", 
	p1.x, p1.y, p2.x, p2.y, result);

	return 0;
}

/* return the Euclidean distance between the given vectors
*/
double
distance(vector_t p1, vector_t p2) {
	double result = 0.0;
	double deltax, deltay;
	deltax = p1.x - p2.x;
	deltay = p1.y - p2.y;
	result = sqrt(deltax*deltax + deltay*deltay);
	return result;
}
