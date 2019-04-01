/** assmt1.c
 *
 * Created by Jiayin Cai
 * (jiayinc@student.unimelb.edu.au)
 * 18/09/2017
 *
 * Input a list of accidents and output statistics calculated on it.
 *
 * To run the program type :
 * ./assmt1 < file
 * Or
 * ./assmt1, then type data
 *
 * C programming is fun
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#define SYMBOL_BOUND 10
#define MAX_LEN_DATE 8
#define MAX_LEN_WEEK 10
#define STAGE_1 1
#define STAGE_2 2
#define STAGE_3 3
#define REFERENCE_LAT 144.963123
#define REFERENCE_LONG -37.810592
#define DIST_CON 6371
#define SYMBOL_BOUND 10
#define EXIT_FAILURE 1
#define MON 1
#define TUES 2
#define WED 3
#define THURS 4
#define FRI 5
#define SAT 6
#define SUN 7

typedef struct list_s{
    int id;
    double x;
    double y;
    char *date;
    int time;
    char *week;
    double dist_ref;
    struct list_s *next;
}list_t;

typedef struct week_s{
    char *day_in_char;
    int day;
    int count;
    struct week_s *next;
}week_t;

void malloc_fail(void);
void print_stage_header(int stage);
void print_graph(double distance);
double to_radian(double x);
double dist(double lat1, double lat2, double long1, double long2);
list_t *add_to_list(list_t *root, int id, double x, double y, char *date,
                    int time, char *week);
list_t *read_all(list_t *root);
int get_list_size(list_t *root);
int corresponding_day(char *week);
week_t *most_acci_day(week_t *root);
week_t *add_to_week(week_t *root, char *week);

void stage_one(list_t *root);
void stage_two(list_t *root);
void stage_three(list_t *root);

/**
 * Main function
 */
int main(int argc, char *argv[]){
    list_t *root = NULL;
    root = read_all(root);
    stage_one(root);
    stage_two(root);
    stage_three(root);
    return 0;
}

/**
 * Stage one of the project
 * Print the first accident information
 * Parameters:
 *      root(list_t): the root of the list
 * Return:
 *      Void
 */
void stage_one(list_t *root){
    print_stage_header(STAGE_1);
    if (root!=NULL){
        printf("Accident: #%d\n", root->id);
        printf("Location: <%lf, %lf>\n", root->x, root->y);
        printf("Date: %s\n", root->date);
        printf("Time: %02d\n", root->time);
        printf("Distance to reference: %05.2lf\n", root->dist_ref);
    }
}

/**
 * Stage two of the project
 * Print accident information with graph
 * Parameters:
 *      root(list_t): the root of the list
 * Return:
 *      Void
 */
void stage_two(list_t *root){
    list_t *current = root;
    print_stage_header(STAGE_2);
    while (current!=NULL){
        printf("Accident: #%d, ", current->id);
        printf("distance to reference: %05.2lf |", current->dist_ref);
        print_graph(current->dist_ref);
        current = current->next;
    }
}

/**
 * Stage three of the project
 * Print the overall accident report
 * Parameters:
 *      root(list_t): the root of the list
 * Return:
 *      Void
 */
void stage_three(list_t *root){
    week_t *week_root = NULL, *most_day;
    list_t *current = root;
    print_stage_header(STAGE_3);
    printf("Number of accidents: %d\n", get_list_size(root));
    while (current!=NULL){
        week_root = add_to_week(week_root, current->week);
        current = current->next;
    }
    most_day = most_acci_day(week_root);
    printf("Day of week with the most accidents: %s (%d accident(s))\n",
            most_day->day_in_char, most_day->count);
}

/**
 * Print the visual graph (- and +), each 10 symbols prints a "+"
 * Parameters:
 *      distance(double): the distance between 2 places
 * Return:
 *      Void
 */
void print_graph(double distance){
    int symbol;
    int num_symbol = ceil(distance);
    for (symbol=1; symbol<=num_symbol; symbol++){
        if ((symbol % SYMBOL_BOUND) != 0){
            printf("-");
        }else{
            printf("+");
        }
    }
    printf("\n");
}

/**
 * Calculate distance between two points
 * Parameters:
 *      lat1(double): the latitude of first point
 *      lat2(double): the latitude of second point
 *      long1(double): the longitude of first point
 *      long2(double): the longitude of second point
 * Return:
 *      double: the distance
 */
double dist(double lat1, double lat2, double long1, double long2){
    double part_1 =  (to_radian(lat2 - lat1)/2) * (to_radian(lat2 - lat1)/2);
    double part_2 =  cos(to_radian(lat1)) * cos(to_radian(lat2));
    double part_3 =  (sin(to_radian(long2 - long1)/2))
                       * (sin(to_radian(long2 - long1)/2));
    double chord_length = part_1 + part_2 * part_3;
    double angle_distance = 2 * atan2(sqrt(chord_length),
                                      sqrt(1 - chord_length));
    return DIST_CON * angle_distance;
}

/**
 * Convert to radian
 * Parameters:
 *      x(double): the number needs to be convert
 * Return:
 *      double: the radian result
 */
double to_radian(double x){
    return x * (3.14159/180);
}

/**
 * Print the header of each stage
 * Parameters:
 *      stage(int): The stage that you need to print the header
 * Return:
 *      Void
 */
void print_stage_header(int stage){
    if (stage != STAGE_1){
        printf("\n");
    }
    printf("Stage %d\n", stage);
    printf("==========\n");
}



/**
 * Find a day that has most accidents
 * Parameters:
 *      root(week_t): the root of the week list
 * Return:
 *      week_t: the item of week day information
 */
week_t *most_acci_day(week_t *root){
    week_t *current = root;
    week_t *temp_max = root;
    while (current!=NULL){
        if (current->count > temp_max->count){
            temp_max = current;
        }
        if (current->count == temp_max->count){
            if (current->day < temp_max->day){
                temp_max = current;
            }
        }
        current = current->next;
    }
    return temp_max;
}

/**
 * Add an item into the week list
 * Parameters:
 *      root(week_t): the root of the list
 *      week(char): the weekday of data
 * Return:
 *      week_t: a list containing new item
 */
week_t *add_to_week(week_t *root, char *week){
    if (root!=NULL){
        if (corresponding_day(week) == root->day){
            root->count++;
        }else{
            root->next = add_to_week(root->next, week);
        }
    }else{
        if ((root = (week_t*)malloc(sizeof(week_t))) == NULL) {
            malloc_fail();
        }
        if ((root->day_in_char =
             (char*)malloc(MAX_LEN_WEEK*sizeof(char))) == NULL) {
            malloc_fail();
        }
        strcpy(root->day_in_char, week);
        root->day = corresponding_day(week);
        root->count = 1;
        root->next = NULL;
    }
    return root;
}

/**
 * Get the corresponding number from a
 * string of week day
 * Parameters:
 *      week(char *): the week day in string
 * Return:
 *      int: the corresponding number
 */
int corresponding_day(char *week){
    if (strcmp("Monday", week)==0) return MON;
    if (strcmp("Tuesday", week)==0) return TUES;
    if (strcmp("Wednesday", week)==0) return WED;
    if (strcmp("Thursday", week)==0) return THURS;
    if (strcmp("Friday", week)==0) return FRI;
    if (strcmp("Saturday", week)==0) return SAT;
    if (strcmp("Sunday", week)==0) return SUN;
    return -1;
}

/**
 * Read all information from input and
 * stores into a list
 * Parameters:
 *      root(list_t): the root of the list
 * Return:
 *      list_t: a list containing new items
 */
list_t *read_all(list_t *root){
    int id = 0, time = 0;
    double x = 0.0, y = 0.0;
    char date[MAX_LEN_DATE], week[MAX_LEN_WEEK];
    while (scanf("%d %lf %lf %s %d %s", &id, &x, &y, date, &time, week) == 6){
        root = add_to_list(root, id, x, y, date, time, week);
    }
    return root;
}

/**
 * Add an item into the list
 * Parameters:
 *      root(list_t): the root of the list
 *      id(int): the id of data
 *      x(double): the longitude of data
 *      y(double): the latitude of data
 *      date(char): the date of data
 *      time(int): the time of data
 *      week(char): the weekday of data
 * Return:
 *      list_t: a list containing new item
 */
list_t *add_to_list(list_t *root, int id, double x, double y, char *date,
                    int time, char *week){
    if (root!=NULL){
        root->next = add_to_list(root->next, id, x, y, date, time, week);
    }else{
        if ((root = (list_t*)malloc(sizeof(list_t))) == NULL) {
            malloc_fail();
        }
        if ((root->date = (char*)malloc(MAX_LEN_DATE*sizeof(char))) == NULL) {
            malloc_fail();
        }
        if ((root->week = (char*)malloc(MAX_LEN_WEEK*sizeof(char))) == NULL) {
            malloc_fail();
        }
        root->id = id;
        root->x = x;
        root->y = y;
        root->time = time;
        strcpy(root->date, date);
        strcpy(root->week, week);
        root->dist_ref = dist(y, REFERENCE_LONG, x, REFERENCE_LAT);
        root->next = NULL;
    }
    return root;
}

/**
 * Get the size of linked list
 * Parameters:
 *      root(list_t): the root of the list
 * Return:
 *      int: the number of items in list
 */
int get_list_size(list_t *root){
    list_t *current = root;
    int count = 0;
    while (current!=NULL){
        count++;
        current = current->next;
    }
    return count;
}

/**
 * Print error status and exit program when malloc fails
 * Parameters:
 *      Void
 * Return:
 *      Void
 */
void malloc_fail(void){
    printf("malloc error\n");
    fflush(stdout);
    exit(EXIT_FAILURE) ;
}
