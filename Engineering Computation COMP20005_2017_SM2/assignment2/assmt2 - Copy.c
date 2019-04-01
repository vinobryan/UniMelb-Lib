/* assmt2.c
 *
 * Created by Ziren Xiao 675485
 * (zirenx@student.unimelb.edu.au)
 * 08/10/2016
 *
 * Input a list of movie gross earnings data, and output
 * statistics calculated on it.
 *
 * To run the program type :
 * ./assmt2 < input_file
 * Or
 * ./assmt2, then type data
 *
 * C programming is fun
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>

#define TRUE 1
#define FALSE 0
#define STAGE_1 1
#define STAGE_2 2
#define STAGE_3 3
#define STAGE_4 4
#define STAGE_1_ID 'W'
#define STAGE_2_ID 'P'
#define ORIGIN_POINT_X 0.0
#define ORIGIN_POINT_Y 0.0
#define FSPL_CONSTANT_1 20
#define FSPL_CONSTANT_2 32.45
#define MAX_WAPS 99
#define LINE_MAX 100
#define INPUT_CHAR_ERROR -1
#define SEPARATOR " "
#define STAGE_3_SQUARE_SIZE 78
#define STAGE_4_SQUARE_SIZE 78
#define STAGE_3_THRESHOLD -50
#define PERCENTAGE 100
#define BOUNDARY_1 0
#define BOUNDARY_2 -10
#define BOUNDARY_3 -20
#define BOUNDARY_4 -30
#define BOUNDARY_5 -40
#define BOUNDARY_6 -50
#define BOUNDARY_1_SYMBOL "+"
#define BOUNDARY_2_SYMBOL " "
#define BOUNDARY_3_SYMBOL "2"
#define BOUNDARY_4_SYMBOL " "
#define BOUNDARY_5_SYMBOL "4"
#define BOUNDARY_6_SYMBOL " "
#define BOUNDARY_OUT_SYMBOL "-"

typedef struct store_s{
    double x_cor;
    double y_cor;
    double trans_power;
    double signal_frequency;
}store_t;

/* Function declaration */
store_t **write_data(store_t **data_store, double x, double y,
                         double trans_power, double signal_f, int count);
char *read_data();
void malloc_fail();
void print_stage_header(int stage);
void stage_four(store_t **data_store, int max_number);
void stage_three(store_t **data_store, int max_number);
void stage_two(store_t **data_store, double x, double y, int max_number);
void stage_one(store_t **data_store, int count);
double max_signal_all(store_t **data_store, double x, double y, int max_number);

/*
 * Main function
 */
int main(int argc, char *argv[]){
    char *temp_read;
    store_t **data_store;
    double x_cor, y_cor, trans_power, signal_frequency;
    int count = 0;
    int stage_1_end = FALSE;
    /* Allocate memory */
    if ((data_store = (store_t**)malloc(MAX_WAPS * sizeof(store_t*))) == NULL) {
            malloc_fail();
    }
    while ((temp_read = read_data()) != NULL){
        /* Stage one start here */
        /* Split string into specific data */
        strtok(temp_read, SEPARATOR);
        x_cor = atof(strtok(NULL, SEPARATOR));
        y_cor = atof(strtok(NULL, SEPARATOR));
        if (temp_read[0] == STAGE_1_ID && stage_1_end == 0){
            trans_power = atof(strtok(NULL, SEPARATOR));
            signal_frequency = atof(strtok(NULL, SEPARATOR));
            data_store = write_data(data_store, x_cor, y_cor,
                                     trans_power, signal_frequency, count);
            count++;
        }else if (stage_1_end == FALSE){
            stage_one(data_store, count);
            print_stage_header(STAGE_2);
            stage_1_end = TRUE;
        }
        /* Stage two start here */
        if (temp_read[0] == STAGE_2_ID){
            stage_two(data_store, x_cor, y_cor, count);
        }else if (stage_1_end == TRUE){
            break;
        }
    }
    stage_three(data_store, count);
    stage_four(data_store, count);
    return 0;
}

/*
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

/*
 * Print error status and exit program when malloc fails
 * Parameters:
 *      Void
 * Return:
 *      Void
 */
void malloc_fail(){
    printf("malloc error\n");
    fflush(stdout);
    exit(1) ;
}

/*
 * Calculate FSPL
 * Parameters:
 *      distance(double): The distance calculated by distance_calc
 *      frequency(double): The corresponding frequency of the distance
 * Return:
 *      (double) The value of FSPL
 */
double FSPL(double distance, double frequency){
     return FSPL_CONSTANT_1 * log10(distance)
            + FSPL_CONSTANT_1 * log10(frequency) + FSPL_CONSTANT_2;
}

/*
 * Calculate distance between 2 points
 * Parameters:
 *      x1(double): X coordinate of the first point
 *      x2(double): X coordinate of the second point
 *      y1(double): Y coordinate of the first point
 *      y2(double): Y coordinate of the second point
 * Return:
 *      (double) The value of distance
 */
double distance_calc(double x1, double x2, double y1, double y2){
    double x = x1 - x2;
    double y = y1 - y2;
    return sqrt(x * x + y * y);
}

/*
 * Calculate absolute value of a number
 * Parameters:
 *      number(double): The number that needs to calculate absolute value
 * Return:
 *      (double) absolute value of the number
 */
double abs_double(double number){
    if (number < 0){
        return -number;
    }else if (number > 0){
        return number;
    }else{
        return 0;
    }
}

/*
 * Calculate maximum signal strength in all WAP points
 * Parameters:
 *      data_store(store_t): The array that stores all WAPs
 *      x(double): X coordinate of a specific point that needs to
 *                  get max signal value
 *      y(double): Y coordinate of a specific point that needs to
 *                  get max signal value
 *      max_number(int): The total number of data in the array
 * Return:
 *      (double) The maximum signal strength
 */
double max_signal_all(store_t **data_store, double x, double y, int max_number){
    int count = 0;
    double max_signal = DBL_MAX, distance, current_signal_strength;
    while (count < max_number){
        distance = distance_calc(data_store[count]->x_cor,
                                  x, data_store[count]->y_cor, y);
        current_signal_strength = data_store[count]->trans_power
                                    - FSPL(distance, data_store[count]->signal_frequency);
        /* Find maximum signal strength at a specific point */
        if (abs_double(current_signal_strength) < abs_double(max_signal)){
            max_signal = current_signal_strength;
        }
        count++;
    }
    return max_signal;
}

/*
 * Read a line of string (end by '\n')
 * Parameters:
 *      Void
 * Return:
 *      (char) The string of inputs
 */
char *read_data(){
    char *data;
    char temp_char;
    int count = 0;
    if ((data = malloc(LINE_MAX * sizeof(char))) == NULL) {
        malloc_fail();
    }
    while ((temp_char = getchar()) != INPUT_CHAR_ERROR){
        if (temp_char!='\n'){
            data[count] = temp_char;
            count++;
        } else {
            data[count] = '\0';
            return data;
        }
    }
    return data;
}

/*
 * Store data into an array
 * Parameters:
 *      data_store(store_t): The array that needs to store data
 *      x(double): The X coordinate that needs to store into array
 *      y(double): The Y coordinate that needs to store into array
 *      trans_power(double): The trans power that needs to store into array
 *      signal_f(double): The signal frequency that needs to store into array
 *      count(int): Position of array of storing
 * Return:
 *      (store_t) The completed storing array
 */
store_t **write_data(store_t **data_store, double x, double y,
                        double trans_power, double signal_f, int count){
    if ((data_store[count] = (store_t*)malloc(sizeof(store_t))) == NULL) {
            malloc_fail();
    }
    /* Store data into array */
    data_store[count]->x_cor = x;
    data_store[count]->y_cor = y;
    data_store[count]->trans_power = trans_power;
    data_store[count]->signal_frequency = signal_f;
    /* End storing */
    return data_store;
}

/*
 * Stage one of the project, output a summary of the WAPs data
 * Parameters:
 *      data_store(store_t): The array that stores WAPs data
 *      max_number(int): The max number of WAPs in the array
 * Return:
 *      (Void)
 */
void stage_one(store_t **data_store, int max_number){
    double max_signal = max_signal_all(data_store, ORIGIN_POINT_X,
                                        ORIGIN_POINT_Y, max_number);
    print_stage_header(STAGE_1);
    printf("Number of WAPs: %02d\n", max_number);
    printf("Maximum signal strength at (00.0, 00.0): %05.2lf dBm\n",
            max_signal);
}

/*
 * Stage two of the project, output max signal strength at a specific point
 * Parameters:
 *      x(double): X coordinate of the point
 *      y(double): Y coordinate of the point
 *      data_store(store_t): The array that stores WAPs data
 *      max_number(int): The max number of WAPs in the array
 * Return:
 *      (Void)
 */
void stage_two(store_t **data_store, double x, double y, int max_number){
    double max_signal = max_signal_all(data_store, x, y, max_number);
    printf("Maximum signal strength at (%04.1lf, %04.1lf): %05.2lf dBm\n",
            x, y, max_signal);
}

/*
 * Stage three of the project, output a summary specific area of signal
 * strength details, comparing with WAPs read before
 * Parameters:
 *      data_store(store_t): The array that stores WAPs data
 *      max_number(int): The max number of WAPs in the array
 * Return:
 *      (Void)
 */
void stage_three(store_t **data_store, int max_number){
    int row, col, count = 0;
    int total_number_of_point, max_stength;
    double percentage_threshold;
    for (row = 1; row < STAGE_3_SQUARE_SIZE; row++){
        for (col = 1; col < STAGE_3_SQUARE_SIZE; col++){
            max_stength = max_signal_all(data_store, row, col, max_number);
            if (max_stength <= STAGE_3_THRESHOLD){
                count++;
            }
        }
    }
    total_number_of_point = (STAGE_3_SQUARE_SIZE - 1)
                            * (STAGE_3_SQUARE_SIZE - 1);
    percentage_threshold = (double)count / total_number_of_point * PERCENTAGE;
    print_stage_header(STAGE_3);
    printf("%d points sampled\n", total_number_of_point);
    printf("%.4d points (%05.2lf%%) with maximum signal strength <= %d dBm\n",
            count, percentage_threshold, STAGE_3_THRESHOLD);
}

/*
 * Stage four of the project, draw a 'signal strength map' with a grid
 * of character.
 * Parameters:
 *      data_store(store_t): The array that stores WAPs data
 *      max_number(int): The max number of WAPs in the array
 * Return:
 *      (Void)
 */
void stage_four(store_t **data_store, int max_number){
    double row, col;
    double current;
    print_stage_header(STAGE_4);
    for (col = STAGE_4_SQUARE_SIZE - 1; col > 0; col = col - 2){
        for (row = 0.5; row <= STAGE_4_SQUARE_SIZE; row++){
            current = max_signal_all(data_store, row, col, max_number);
            if (current > BOUNDARY_1){
                printf("%s", BOUNDARY_1_SYMBOL);
            }else if (current > BOUNDARY_2){
                printf("%s", BOUNDARY_2_SYMBOL);
            }else if (current > BOUNDARY_3){
                printf("%s", BOUNDARY_3_SYMBOL);
            }else if (current > BOUNDARY_4){
                printf("%s", BOUNDARY_4_SYMBOL);
            }else if (current > BOUNDARY_5){
                printf("%s", BOUNDARY_5_SYMBOL);
            }else if (current > BOUNDARY_6){
                printf("%s", BOUNDARY_6_SYMBOL);
            }else{
                printf("%s", BOUNDARY_OUT_SYMBOL);
            }
        }
        printf("\n");
    }
}
