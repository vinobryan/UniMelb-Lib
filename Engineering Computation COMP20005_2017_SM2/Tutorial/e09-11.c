/* Exercise 9.11: Compute the maximum altitude reached by a toy rocket
   and the duration of the rocket's flight.
   Jianzhong Qi and Alistair Moffat, May 2013; corrections JUne 2014
 */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#define PLOT_CHARS	72	/* width of plot in characters */
#define PLOT_LINES	40	/* number of rows in output plot */

#define G		9.81  	/* standard gravity constant, m/s^2 */
#define ROCKET_WEIGHT	10.0	/* dry rocket weight, kg */
#define FUEL_WEIGHT	8.0	/* initial fuel weight, kg */
#define FUEL_RATE	0.8	/* fuel consumption rate, kg/s */
#define FUEL_FORCE	500.0	/* Thrust generated, Newtons */
#define K		0.6	/* air resistance constant, kg/m^3 */
#define A		0.1	/* cross-sectional area of the rocket, m^2 */

#define MIN_Y 		0 	/* presumed min Y */

#define ROCKET_FLAME	">>=>"	/* the rocket emitting flame */
#define ROCKET_UP	"===>"	/* the rocket still going up */
#define ROCKET_DOWN	"<==="	/* the rocket coming down */

void getMaxAltitudeAndDuration (double deltat, double* maxY, double* t);
void drawJourney(double deltat, double maxY, double duration);
void drawRocket(double t, double fracy, double v, int hasFuel);

int
main(int argc, char *argv[]) {
	double maxT;
	double maxY = 0;
	double deltat;
	
	if (argc!=2) {
		printf("Usage: %s deltat\n", argv[0]);
		exit(EXIT_FAILURE);
	}
	deltat = atof(argv[1]);
	
	/* get the maximum altitude and the journey time */
	getMaxAltitudeAndDuration(deltat, &maxY, &maxT);
	
	/* then plot exactly the same journey */
	drawJourney(deltat, maxY, maxT);
	
	/* output the desired result */
	printf("Maximum altitude: %.2f metres, duration %.2f seconds\n",
		maxY, maxT);
	
	return 0;
}

/* get the maximum altitude the rocket can reach and the duration of
   the journey.
*/
void
getMaxAltitudeAndDuration (double deltat, double* maxY, double* t) {
	double a, v, y, m;
	
	y = 0.0;
	m = ROCKET_WEIGHT + FUEL_WEIGHT;
	v = 0.0;
	*t = 0.0;
	while ((y>MIN_Y) || (m>ROCKET_WEIGHT)) {
		if (m > ROCKET_WEIGHT) {
			/* still fuel left */
			a = (1/m)*(FUEL_FORCE - m*G - K*A*v*fabs(v));
		} else {
			/* now in free fall */
			a = (1/m)*(-m*G - K*A*v*fabs(v));			
		}
		y += deltat * v;
		v += deltat * a;
		*t += deltat;
		if (m-FUEL_RATE*deltat > ROCKET_WEIGHT) {
			m -= FUEL_RATE*deltat;
		} else {
			m = ROCKET_WEIGHT;
		}
		if (y > *maxY) {
			*maxY = y;
		}
	}
	return;
}

/* now animate the same journey */
void
drawJourney(double deltat, double maxY, double duration) {
	double a, v, y, m, t;
	int hasFuel = 1;
	int nextLine = 0;
	
	printf("Seconds\n");
	y = 0.0;
	m = ROCKET_WEIGHT + FUEL_WEIGHT;
	v = 0.0;
	t = 0.0;
	
	/* duplicate code, would be nice to not have it like this */
	while ((y>MIN_Y) || (m>ROCKET_WEIGHT)) {
		if (t/duration*(PLOT_LINES-1) >= nextLine) {
			drawRocket(t, y/maxY, v, hasFuel);
			nextLine += 1;
		}
			
		
		if (m > ROCKET_WEIGHT) {
			a = (1/m)*(FUEL_FORCE - m*G - K*A*v*fabs(v));
		} else {
			a = (1/m)*(-m*G - K*A*v*fabs(v));
			hasFuel = 0;
		}
		y += deltat * v;
		v += deltat * a;
		t += deltat;
		if (m-FUEL_RATE*deltat > ROCKET_WEIGHT) {
			m -= FUEL_RATE*deltat;
		} else {
			m = ROCKET_WEIGHT;
		}
	}
	drawRocket(t, 0, 0, hasFuel);
}

/* draw one line of the plot */
void
drawRocket(double t, double fracy, double v, int hasFuel) {
	int i;
	printf("%4.1f|", t);
	for (i=0; i < (PLOT_CHARS-strlen(ROCKET_UP))*fracy; i++) {
		printf(" ");
	}
	if (hasFuel) {
		printf(ROCKET_FLAME);
	} else if (v>0) {
		printf(ROCKET_UP);
	} else {
		printf(ROCKET_DOWN);
	}
	printf("\n");
	return;
}
