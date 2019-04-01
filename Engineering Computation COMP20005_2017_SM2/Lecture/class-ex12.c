/* Starting point for Gaussian elimination on fixed matrix
* Alistair Moffat, June 2013.
* Modified by Jianzhong Qi, November 2015, based on class exercise descriptions
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define EPS 1e-6

#define MAX 100

void make_upper_triangular(double A[][MAX+1], int n);
void pivot_matrix(double A[][MAX+1], int n, int i);
void swap_matrix_rows(double A[][MAX+1], int n, int i, int r);
void print_eqs(double A[][MAX+1], int n, char *msg);

int
main(int argc, char *argv[]) {
	
	int n=3;
	double A[MAX][MAX+1] =
	{{0,2,3,1},{3,5,6,2},{9,2,3,3}};
	/* that is, 0x_0 + 2x_1 + 3x_2 = 1, etc */
	
	print_eqs(A, n, "Starting");
	make_upper_triangular(A, n);
	print_eqs(A, n, "Final");
	
	return 0;
}

void
make_upper_triangular(double A[][MAX+1], int n) {
	int i;
	/* forwards stage, to build into upper triangular */
	for (i=0; i<n; i++) {
		pivot_matrix(A, n, i);
	}
	
	/* Now in upper triangular form, next step is the 
	back substitution */
	
	return;
}

void 
pivot_matrix(double A[][MAX+1], int n, int i) {
	int j, r, c;
	double pivotval;
	/* identify the pivot row */
	r = i;
	pivotval = A[i][i];
	for (j=i+1; j<n; j++) {
		if (fabs(A[j][i]) > fabs(A[r][i])) {
			pivotval = A[j][i];
			r = j;
		}
	}
	/* swap the i'th and r'th rows over */
	swap_matrix_rows(A, n, i, r);
	print_eqs(A, n, "After row swap");
	
	/* check that pivot value is safe, error and exit if not */
	if (fabs(A[i][i]) < EPS) {
		printf("System does not have simple solution\n\n");
		exit(EXIT_FAILURE);
	}
	
	/* normalise the i'th row */
	for (c=i; c<=n; c++) {
		A[i][c] = A[i][c]/pivotval;
	}
	print_eqs(A, n, "After row normalization");
	
	/* and now adjust the other rows below the i'th row */
	for (j=i+1; j<n; j++) {
		pivotval = -A[j][i]/A[i][i];
		for (c=i; c<=n; c++) {
			A[j][c] = A[j][c] + pivotval*A[i][c];
		}
		print_eqs(A, n, "After elimination");
	}
}


void
swap_matrix_rows(double A[][MAX+1], int n, int i, int r) {
	double tmp;
	int j;
	for (j=i; j<n+1; j++) {
		tmp = A[i][j];
		A[i][j] = A[r][j];
		A[r][j] = tmp;
	}
}

void
print_eqs(double A[][MAX+1], int n, char *msg) {
	int i, j;
	printf("%s:\n", msg);
	for (i=0; i<n; i++) {
		for (j=0; j<n; j++) {
			printf("%6.3fx_%d", A[i][j], j);
			if (j<n-1) {
				printf(" + ");
			} else {
				printf(" = ");
			}
		}
		printf("%6.3f\n", A[i][n]);
	}
	printf("\n");
	return;
}
