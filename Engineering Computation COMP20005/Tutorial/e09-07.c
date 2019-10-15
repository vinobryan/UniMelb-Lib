/*
 * Exercise 9.7 -- Maximum subset sum problem
 *
 * Author:  Kancelot To
 * Date:    26/05/2015
 */

#include <stdio.h>
#include <stdlib.h>

int max_subarray(int[], int, int*, int*);
int max_subarray_kadane(int A[], int n, int *from, int *to);
void print_array(int[], int, int);

int
main(int argc, char *argv[]) {
    int from=0;
    int to=0;
    int sum;

    int A[] = {5, -6, 5, 3, -2, 3, -1, 4};
    int n_A = 8;

    printf("orig array:\n");
    print_array(A, 0, n_A - 1);

    /* sum = max_subarray(A, n_A, &from, &to); */
    sum = max_subarray_kadane(A, n_A, &from, &to);

    printf("sub array:\n");
    print_array(A, from, to);
    printf("sum = %d\n\n", sum);
    
    return 0;
}

int
max_subarray(int A[], int n, int *from, int *to) {
    int sum;
    int max_sum = 0;
    int i;
    int j;

    /* Brute force approach */
    for (i = 0; i < n; i++) {
        sum = 0;

        for (j = i; j < n; j++) {
            sum += A[j];

            if (sum > max_sum) {
                max_sum = sum;
                *from = i;
                *to = j;
            }
        }
    }

    return max_sum;
}

int
max_subarray_kadane(int A[], int n, int *from, int *to) {
    int sum = 0;
    int max_sum = 0;
    int start_index = 0;
    int i;

    /* Kadane's algorithm -- only needs one pass through the array */
    for (i = 0; i < n; i++) {
        sum += A[i];
        if (sum < 0) {
            /* this number gives a net loss to our sum -- 
             * reset it back to zero */
            sum = 0;
            start_index = i + 1;
        }

        if (sum > max_sum) {
            /* this number is beneficial to our total sum */
            max_sum = sum;
            *from = start_index;
            *to = i;
        }
    }

    return max_sum;
}

void
print_array(int A[], int from, int to) {
    int i;
    for (i = from; i <= to; i++) {
        printf("%4d", A[i]);
    }
    printf("\n");
}
