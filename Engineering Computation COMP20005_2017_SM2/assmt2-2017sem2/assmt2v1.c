/** assmt2.c
 *
 * Created by XXXX
 * (XXX@student.unimelb.edu.au)
 * 02/10/2017
 *
 * Input a list of movie gross earnings data, and output
 * statistics calculated on it.
 *
 * To run the program type :
 * ./assmt2 < input_file
 *
 * C programming is fun
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>

#define MAX_SOUNDS 99
#define MAX_POINTS 99
#define ORIGIN_X 0.0
#define ORIGIN_Y 0.0
#define STAGE_1_ID 'S'
#define STAGE_2_ID 'P'
#define STAGE_1 1
#define STAGE_2 2
#define STAGE_3 3
#define STAGE_4 4
#define DEFAULT_DISTANCE 1
#define STAGE_3_SIZE 312
#define STAGE_3_GRID 4
#define STAGE_3_THRESHOLD 55
#define STAGE_4_SIZE 78
#define STAGE_4_X_START 2
#define STAGE_4_Y_START 308
#define STAGE_4_X_END 310
#define STAGE_4_Y_END 4
#define STAGE_4_HIGHT_GRID 8
#define STAGE_4_WIDTH_GRID 4
#define PERCENTAGE 100
#define BOUNDARY_1 100
#define BOUNDARY_2 90
#define BOUNDARY_3 80
#define BOUNDARY_4 70
#define BOUNDARY_5 60
#define BOUNDARY_6 55

typedef struct store_s{
    double x;
    double y;
    double sound_level;
}store_t;

store_t **write_data(store_t **sounds, double x, double y, double level,
                    int count);
void malloc_fail();
void print_stage_header(int stage);
double distance(double x1, double x2, double y1, double y2);
double equation_one(double level, double dist1, double dist2);
double equation_two(store_t **data, double x, double y, int max);
void stage_one(store_t **sounds, int count);
void stage_two(store_t **sounds, double x, double y, int max);
void stage_three(store_t **sounds, int max);
void stage_four(store_t **sounds, int max);

/** Main function
 */
int main(int argc, char *argv[]){
    store_t **sounds;
    int count = 0;
    int stage_1_end = false;
    /* Allocate memory */
    if ((sounds = (store_t**)malloc((MAX_SOUNDS + MAX_POINTS)
                                         * sizeof(store_t*))) == NULL) {
            malloc_fail();
    }
    while (true){
        double x, y, level;
        char id = getchar();
        if (id == STAGE_1_ID && !stage_1_end){
            scanf("%lf %lf %lf\n", &x, &y, &level);
            sounds = write_data(sounds, x, y, level, count);
            count++;
        }else if (!stage_1_end){
            stage_one(sounds, count);
            print_stage_header(STAGE_2);
            stage_1_end = true;
        }
        if (id == STAGE_2_ID){
            scanf("%lf %lf\n", &x, &y);
            stage_two(sounds, x, y, count);
        }else if (stage_1_end){
            break;
        }
    }
    stage_three(sounds, count);
    stage_four(sounds, count);
    return 0;
}

/**
 * Store data into an array
 * Parameters:
 *      sounds(store_t): The array that needs to store data
 *      x(double): The X coordinate that needs to store into array
 *      y(double): The Y coordinate that needs to store into array
 *      level(double): The sound level that needs to store into array
 *      count(int): Position of array of storing
 * Return:
 *      (store_t) The completed storing array
 */
store_t **write_data(store_t **sounds, double x, double y, double level,
                    int count){
    if ((sounds[count] = (store_t*)malloc(sizeof(store_t))) == NULL) {
            malloc_fail();
    }
    sounds[count]->x = x;
    sounds[count]->y = y;
    sounds[count]->sound_level = level;
    return sounds;
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

/**
 * Calculate distance between 2 points
 * Parameters:
 *      x1(double): X coordinate of the first point
 *      x2(double): X coordinate of the second point
 *      y1(double): Y coordinate of the first point
 *      y2(double): Y coordinate of the second point
 * Return:
 *      (double) The value of distance
 */
double distance(double x1, double x2, double y1, double y2){
    double x = x1 - x2;
    double y = y1 - y2;
    return sqrt(x * x + y * y);
}

/**
 * Calculate aggregate sound level
 * Parameters:
 *      sounds(store_t): The array that stores loudspeakers data
 *      x(double): X coordinate of the second point
 *      y(double): Y coordinate of the first point
 *      count(int): The number of sounds in the array
 * Return:
 *      (double) The aggregate sound level
 */
double equation_two(store_t **data, double x, double y, int count){
    int index;
    double sum = 0;
    for (index=0; index<count; index++){
        double dist = distance(data[index]->x, x, data[index]->y, y);
        double result = equation_one(data[index]->sound_level,
                                    DEFAULT_DISTANCE, dist);
        sum = sum + pow(10, result/10);
    }
    return 10 * log10(sum);
}

/**
 * Calculate sound level in new distance, invalid
 * data will return 0.
 * Parameters:
 *      level(double): the old level of sound
 *      dist1(double): the original distance
 *      dist2(double): the new distance
 * Return:
 *      (double) The aggregate sound level
 */
double equation_one(double level, double dist1, double dist2){
    if (level < 0 || dist2 == 0){
        return 0;
    }
    return (level + 20 * log10(dist1/dist2));
}

/**
 * Stage one of the project, output a summary of input
 * Parameters:
 *      sounds(store_t): The array that stores loudspeakers data
 *      count(int): The number of sounds in the array
 * Return:
 *      (Void)
 */
void stage_one(store_t **sounds, int count){
    double sum = equation_two(sounds, ORIGIN_X, ORIGIN_Y, count);
    print_stage_header(STAGE_1);
    printf("Number of loudspeakers: %02d\n", count);
    printf("Sound level at (000.0, 000.0): %05.2lf dB\n", sum);
}

/**
 * Stage two of the project, output sound level of each point
 * Parameters:
 *      x(double): X coordinate of the point
 *      y(double): Y coordinate of the point
 *      sounds(store_t): The array that stores data
 *      max(int): The max number of data in the array
 * Return:
 *      (Void)
 */
void stage_two(store_t **sounds, double x, double y, int max){
    double sum = equation_two(sounds, x, y, max);
    printf("Sound level at (%05.1lf, %05.1lf): %05.2lf dB\n", x, y, sum);
}

/**
 * Stage three of the project, output a summary of 312 x 312 area
 * Parameters:
 *      sounds(store_t): The array that stores WAPs data
 *      max(int): The max number of data in the array
 * Return:
 *      (Void)
 */
void stage_three(store_t **sounds, int max){
    int row, col;
    int total_number_of_point = 0, less_threshold = 0;
    double percentage_threshold;
    for (row = STAGE_3_GRID; row < STAGE_3_SIZE; row++){
        for (col = STAGE_3_GRID; col < STAGE_3_SIZE; col++){
            if ((row % STAGE_3_GRID == 0) && (col % STAGE_3_GRID == 0)){
                double sum = equation_two(sounds, row, col, max);
                if (sum <= STAGE_3_THRESHOLD){
                    less_threshold++;
                }
                total_number_of_point++;
            }
        }
    }
    percentage_threshold =
                (double)less_threshold / total_number_of_point * PERCENTAGE;
    print_stage_header(STAGE_3);
    printf("%d points sampled\n", total_number_of_point);
    printf("%.4d points (%05.2lf%%) have sound level <= %d dB\n",
            less_threshold, percentage_threshold, STAGE_3_THRESHOLD);
}

/**
 * Stage four of the project, draw a 'sound map' with a grid
 * of character.
 * Parameters:
 *      sounds(store_t): The array that stores data
 *      max(int): The max number of data in the array
 * Return:
 *      (Void)
 */
void stage_four(store_t **sounds, int max){
    double row, col;
    print_stage_header(STAGE_4);
    for (row = STAGE_4_Y_START; row >= STAGE_4_Y_END;
        row = row - STAGE_4_HIGHT_GRID){
        for (col = STAGE_4_X_START; col <= STAGE_4_X_END;
            col = col + STAGE_4_WIDTH_GRID){
            double current = equation_two(sounds, col, row, max);
            if (current >= BOUNDARY_1){
                printf("%c", '+');
            }else if (current >= BOUNDARY_2){
                printf("%c", ' ');
            }else if (current >= BOUNDARY_3){
                printf("%c", '8');
            }else if (current >= BOUNDARY_4){
                printf("%c", ' ');
            }else if (current >= BOUNDARY_5){
                printf("%c", '6');
            }else if (current > BOUNDARY_6){
                printf("%c", ' ');
            }else{
                printf("%c", '-');
            }
        }
        printf("\n");
    }
}
