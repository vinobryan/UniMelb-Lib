/*
 * Author: Mischka Kamener
 * Date: May 2015
 * Description:
 *      A programming exercise for question 8.9 (page 139).
 *      A program that sorts staff_t by ascending employeenumber.
 */

#include <stdio.h>
#include <stdlib.h>

/*---------------------------------------------------------------------------*/
#define NAMESTRLEN  40
#define MAXSUBJECTS 8
#define MAXSTAFF    10
#define SEED        129745  /* Need to change seed if you want IDs to change */

/*---------------------------------------------------------------------------*/
typedef char namestr_t[NAMESTRLEN+1];     /* +1 for null byte */

typedef struct
{
    namestr_t   given, others, family;
} fullname_t;

typedef struct
{
    int     yy, mm, dd;
} date_t;

typedef struct
{
    fullname_t  name;
    int         employeenumber;
    date_t      dob;
    date_t      datecommenced;
    int         status;
    int         annualsalary;
} staff_t;

/*---------------------------------------------------------------------------*/
void sort_staff_by_number( staff_t staff[], int nstaff );
void staff_swap( staff_t *a, staff_t *b );

/*---------------------------------------------------------------------------*/
int
main( int argc, char *argv[] )
{
    staff_t all_staff[MAXSTAFF];
    int     nstaff = MAXSTAFF;
    int     i;
    srand(SEED);
    
    /* Set employee ids of staff and display. */
    printf("IDs before sorting:");
    for( i = 0; i < nstaff; i++ )
    {
        /* Generate staff numbers between 1 and 1000. May be duplicates. */ 
        all_staff[i].employeenumber = 1+rand()%1000;
        printf(" %d", all_staff[i].employeenumber);
    }
    printf("\n");
    
    /* Sort staff by employee ids. */
    sort_staff_by_number(all_staff, nstaff);
    
    /* Display order after sorting. */
    printf("IDs after sorting:");
    for( i = 0; i < nstaff; i++ )
    {
        printf(" %d", all_staff[i].employeenumber);
    }
    printf("\n");
    
    return 0;
}

/*---------------------------------------------------------------------------*/
/* Sorts an array of staff_t by ascending employeenumber. Uses a 
 * recursive implementation of the insertion sort algorithm.
 */

void
sort_staff_by_number( staff_t staff[], int nstaff )
{
    int i;
    
    /* Base case, array of size 1 or less is sorted. */
    if (nstaff <= 1)
    {
        return;
    }
    
    /* First sort all elements in array except for the last one. */
    sort_staff_by_number(staff, nstaff-1);
    
    /* Swap last element into correct position. */
    for( i = nstaff-1; i > 0; i-- )
    {
        /* If in correct order, then the last element has been sorted. */
        if( staff[i].employeenumber > staff[i-1].employeenumber )
        {
            return;
        }
        /* Not in correct order, need to swap into position. */
        else
        {
            staff_swap(&staff[i], &staff[i-1]);
        }
    }
    
    return;
}

/*---------------------------------------------------------------------------*/
/* Swaps the position of two staff_t variables. */

void
staff_swap( staff_t *a, staff_t *b )
{
    staff_t tmp = *a;
    *a = *b;
    *b = tmp;
    
    return;
}
