/* Solution to PPSAA q8.5, p138
 * Prepared by Quynh-Chi Nguyen
 * Modified by Alistair Moffat
 * May 2010
 */

#include <stdio.h>

typedef struct {
	double re;
	double im;
} complex_t;

complex_t complex_add(complex_t, complex_t);
complex_t complex_mpy(complex_t, complex_t);
void print_complex(complex_t);

/* simple scaffolding to test functions */
int
main (int argc, char *argv[]) {
	complex_t v1 = {2.0, -3.0};
	complex_t v2 = {3.0,  1.5};
	print_complex(v1);
	print_complex(v2);
	print_complex(complex_add(v1, v2));
	print_complex(complex_mpy(v1, v2));
	return 0;
}

complex_t
complex_add(complex_t v1, complex_t v2) {
	complex_t result;
	result.re = v1.re + v2.re;
	result.im = v1.im + v2.im;
	return result;
}

complex_t
complex_mpy(complex_t v1, complex_t v2) {
	complex_t result;
	result.re = v1.re*v2.re - v1.im*v2.im;
	result.im = v1.im*v2.re + v1.re*v2.im;
	return result;
}

void
print_complex(complex_t v) {
	printf("%6.2f + %6.2fi\n", v.re, v.im);
}
