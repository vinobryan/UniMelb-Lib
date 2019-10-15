/* Exercise 8.3 and 8.4, page 138
   Alistair Moffat, May 2014.
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* two-d points */
typedef struct {
	double x;
	double y;
} vector_t;

/* polygons */
#define MAXPTS 100
typedef struct {
	vector_t pts[MAXPTS];
	int npts; /* the buddy variable */
} poly_t;
	
/* function prototype */
double distance(vector_t, vector_t);
double trapezoid(vector_t, vector_t);
double perimeter(poly_t P);
double area(poly_t P);

int
main(int argc, char *argv[]) {
    poly_t testP = { { {0,0}, {0,4}, {5,4}, {9,0} }, 4 };
    double result;

    result = perimeter(testP);
    printf("perimeter of testP is %.2f\n",  result);
    result = area(testP);
    printf("area      of testP is %.2f\n",  result);
    
    return 0;
}

/* Ex 8.03 -- compute the perimter of a polygon */
double
perimeter(poly_t P) {
	int i;
	double perim=0.0;
	/* step through the edges, adding up their lengths */
	for (i=0; i<P.npts-1; i++) {
		perim += distance(P.pts[i], P.pts[i+1]);
	}
	/* plus the last closing edge */
	perim += distance(P.pts[P.npts-1], P.pts[0]);
	return perim;
}

/* Ex 8.04 -- compute the area of a polygon */
double
area(poly_t P) {
	int i;
	double area=0.0;
	/* step through the edges, adding up the areas of the
	   trapezoidal regions indicated by pairs of consecutive
	   points. Assumes that polygon is well-behaved.
	*/
	for (i=0; i<P.npts-1; i++) {
		area += trapezoid(P.pts[i], P.pts[i+1]);
	}
	/* plus the last closing edge */
	area += trapezoid(P.pts[P.npts-1], P.pts[0]);
	/* area is always positive, but summation may have ended
	   up with a negative value */
	if (area<0) {
		area = -area;
	}
	return area;
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

/* return the area of a trapezoid defined by two points and
   the x axis
 */
double
trapezoid(vector_t p1, vector_t p2) {
	double result = 0.0;
	double deltax, avheight;
	deltax = p2.x - p1.x;
	avheight = (p1.y+p2.y)/2;
	result = deltax * avheight;
	return result;
}
