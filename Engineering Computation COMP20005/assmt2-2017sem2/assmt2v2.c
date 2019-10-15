/** assmt2.c
 *
 * Created by Jiayin Cai
 * (jiayinc@student.unimelb.edu.au)
 * 08/10/2017
 *
 * C programming is fun
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>

#define STAGE_ONE_HEAD 1
#define STAGE_TWO_HEAD 2
#define STAGE_THREE_HEAD 3
#define STAGE_FOUR_HEAD 4
#define EUQATION_ONE_DISTANCE 1
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
#define MAX_SOUNDS 99
#define MAX_POINTS 99
#define STAGE_ONE_FIRST_CHAR 'S'
#define STAGE_TWO_FIRST_CHAR 'P'
#define PERCENTAGE 100
#define THRESHOLD_1 100
#define THRESHOLD_2 90
#define THRESHOLD_3 80
#define THRESHOLD_4 70
#define THRESHOLD_5 60
#define THRESHOLD_6 55

typedef struct location_s{
    double x;
    double y;
}location_t;

typedef struct sounds_s{
    location_t location;
    double sound_level;
}sounds_t;

void store_sounds(sounds_t sounds[], double x, double y, double level,
                int index);
void store_location(location_t *location, double x, double y);
void stage_header(int stage);
double distance(double x1, double x2, double y1, double y2);
double first_equation(double level, double x1, double x2, double y1,
                        double y2);
double second_equation(sounds_t data[], double x, double y, int size_sounds);
void stage_one(sounds_t sounds[], int size_sounds);
void stage_two(sounds_t sounds[], location_t locations[],
                int num_location, int size_sounds);
void stage_three(sounds_t sounds[], int size_sounds);
void stage_four(sounds_t sounds[], int size_sounds);


int main(int argc, char *argv[]){
    sounds_t sounds[MAX_SOUNDS];
    location_t locations[MAX_POINTS];
    int index_sound = 0, index_location = 0;
    while (true){
        double x, y, level;
        char id = getchar();
        if (id == STAGE_ONE_FIRST_CHAR){
            scanf("%lf %lf %lf\n", &x, &y, &level);
            store_sounds(sounds, x, y, level, index_sound);
            index_sound++;
        }else if (id == STAGE_TWO_FIRST_CHAR){
            scanf("%lf %lf\n", &x, &y);
            store_location(&locations[index_location], x, y);
            index_location++;
        }else{
            break;
        }
    }
    stage_one(sounds, index_sound);
    stage_two(sounds, locations, index_location, index_sound);
    stage_three(sounds, index_sound);
    stage_four(sounds, index_sound);
    return 0;
}

/**
 * Store data into an array
 * Parameters:
 *      sounds(sounds_t): The array that needs to store data
 *      x(double): The X coordinate that needs to store into array
 *      y(double): The Y coordinate that needs to store into array
 *      level(double): The sound level that needs to store into array
 *      count(int): Position of array of storing
 * Return:
 *      (sounds_t) The completed storing array
 */
void store_sounds(sounds_t sounds[], double x, double y, double level,
                int index){
    store_location(&(sounds[index].location), x, y);
    sounds[index].sound_level = level;
}

void store_location(location_t *location, double x, double y){
    location->x = x;
    location->y = y;
}

/**
 * Print the header of each stage
 * Parameters:
 *      stage(int): The stage that you need to print the header
 * Return:
 *      Void
 */
void stage_header(int stage){
    if (stage != STAGE_ONE_HEAD){
        printf("\n");
    }
    printf("Stage %d\n", stage);
    printf("==========\n");
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
    return sqrt((x1-x2) * (x1-x2) + (y1-y2) * (y1-y2));
}

/**
 * Calculate aggregate sound level
 * Parameters:
 *      sounds(sounds_t): The array that stores loudspeakers data
 *      x(double): X coordinate of the second point
 *      y(double): Y coordinate of the first point
 *      count(int): The number of sounds in the array
 * Return:
 *      (double) The aggregate sound level
 */
double second_equation(sounds_t data[], double x, double y, int size_sounds){
    int index;
    double sum = 0;
    for (index=0; index<size_sounds; index++){
        double equation_one_result = first_equation(data[index].sound_level,
                                    data[index].location.x, x,
                                    data[index].location.y, y);
        sum = sum + pow(10, equation_one_result/10);
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
double first_equation(double level, double x1, double x2, double y1,
                    double y2){
    double dist = distance(x1, x2, y1, y2);
    return (level + 20 * log10(EUQATION_ONE_DISTANCE/dist));
}

/**
 * Stage one of the project, output a summary of input
 * Parameters:
 *      sounds(sounds_t): The array that stores loudspeakers data
 *      count(int): The number of sounds in the array
 * Return:
 *      (Void)
 */
void stage_one(sounds_t sounds[], int size_sounds){
    double sum = second_equation(sounds, 0.0, 0.0, size_sounds);
    stage_header(STAGE_ONE_HEAD);
    printf("Number of loudspeakers: %02d\n", size_sounds);
    printf("Sound level at (000.0, 000.0): %05.2lf dB\n", sum);
}

/**
 * Stage two of the project, output sound level of each point
 * Parameters:
 *      x(double): X coordinate of the point
 *      y(double): Y coordinate of the point
 *      sounds(sounds_t): The array that stores data
 *      max(int): The max number of data in the array
 * Return:
 *      (Void)
 */
void stage_two(sounds_t sounds[], location_t locations[],
                int num_location, int size_sounds){
    int index;
    stage_header(STAGE_TWO_HEAD);
    for (index=0; index<num_location; index++){
        double x = locations[index].x;
        double y = locations[index].y;
        double sum = second_equation(sounds, x, y, size_sounds);
        printf("Sound level at (%05.1lf, %05.1lf): %05.2lf dB\n", x, y, sum);
    }
}

/**
 * Stage three of the project, output a summary of 312 x 312 area
 * Parameters:
 *      sounds(sounds_t): The array that stores WAPs data
 *      max(int): The max number of data in the array
 * Return:
 *      (Void)
 */
void stage_three(sounds_t sounds[], int size_sounds){
    int row, col;
    int total_number_of_point = 0, small_than_threshold = 0;
    for (row = STAGE_3_GRID; row < STAGE_3_SIZE; row++){
        for (col = STAGE_3_GRID; col < STAGE_3_SIZE; col++){
            if ((row % STAGE_3_GRID == 0) && (col % STAGE_3_GRID == 0)){
                double sum = second_equation(sounds, row, col, size_sounds);
                if (sum <= STAGE_3_THRESHOLD){
                    small_than_threshold++;
                }
                total_number_of_point++;
            }
        }
    }
    stage_header(STAGE_THREE_HEAD);
    printf("%d points sampled\n", total_number_of_point);
    printf("%.4d points (%05.2lf%%) have sound level <= 55 dB\n",
            small_than_threshold,
            (double)small_than_threshold / total_number_of_point * 100);
}

/**
 * Stage four of the project, draw a 'sound map' with a grid
 * of character.
 * Parameters:
 *      sounds(sounds_t): The array that stores data
 *      max(int): The max number of data in the array
 * Return:
 *      (Void)
 */
void stage_four(sounds_t sounds[], int size_sounds){
    double row, col;
    stage_header(STAGE_FOUR_HEAD);
    for (row = STAGE_4_Y_START; row >= STAGE_4_Y_END;
        row = row - STAGE_4_HIGHT_GRID){
        for (col = STAGE_4_X_START; col <= STAGE_4_X_END;
            col = col + STAGE_4_WIDTH_GRID){
            double sound_level = second_equation(sounds, col, row, size_sounds);
            if (sound_level >= THRESHOLD_1){
                printf("%c", '+');
            }else if (sound_level >= THRESHOLD_2){
                printf("%c", ' ');
            }else if (sound_level >= THRESHOLD_3){
                printf("%c", '8');
            }else if (sound_level >= THRESHOLD_4){
                printf("%c", ' ');
            }else if (sound_level >= THRESHOLD_5){
                printf("%c", '6');
            }else if (sound_level > THRESHOLD_6){
                printf("%c", ' ');
            }else{
                printf("%c", '-');
            }
        }
        printf("\n");
    }
}
