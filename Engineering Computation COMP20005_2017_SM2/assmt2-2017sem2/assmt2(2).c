/** assmt2.c
 *
 * Created by Jiayin Cai
 * (jiayinc@student.unimelb.edu.au)
 * 02/10/2017
 *
 * Input a list of movie gross earnings data, and output
 * statistics calculated on it.
 *
 * To run the program type :
 * ./assmt2 < input_file
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>



#define MAX_LOCATION 200
#define STAGE_ONE_ID 'S'
#define STAGE_TWO_ID 'P'
#define ORIGINAL_X 0.0
#define ORIGINAL_Y 0.0
#define STAGE_ONE_INDEX 1
#define STAGE_TWO_INDEX 2
#define STAGE_THREE_INDEX 3
#define STAGE_FOUR_INDEX 4
#define DEFAULT_DISTANCE 1
#define MAX_ROW 312
#define START_ROW 4
#define MAX_COLUME 312
#define START_COL 4
#define EACH_ADD 4
#define THRESHOLD_ONE 55
#define THRESHOLD_TWO 100
#define THRESHOLD_THREE 90
#define THRESHOLD_FOUR 80
#define THRESHOLD_FIVE 70
#define THRESHOLD_SIX 60
#define FOUR_X_START 2
#define FOUR_X_MAX 310
#define FOUR_Y_START 308
#define FOUR_Y_MIN 4
#define FOUR_WIDTH 4
#define FOUR_HIGHT 8

typedef struct sound_s{
    double x;
    double y;
    double sound_level;
}sound_t;

typedef struct summary_s{
    struct sound_s **sound;
    int count;
}summary_t;


sound_t **storeSounds(sound_t **sounds, double x_val, double y_val,
	                  double sound_level, int count);
double calculateOrDecibel (double l2, double r1, double r2);
double calculateSumDecibel (double *l2);
double stageFirstScd(sound_t **sounds,double x, double y,int index, int flag);
void stageThree(sound_t **sounds, int index);
void stageFour(sound_t **sounds, int index);

/** Main function
 */
int main(int argc, char *argv[]){
   sound_t **sound;
   int index = 0;
   int check=0;
   sound = (sound_t**)malloc(MAX_LOCATION * sizeof(sound_t*));
   double x, y, sound_level;
    while (true){
        char stage_id = getchar();
        if(check==0){
        	if(stage_id==STAGE_ONE_ID){
                scanf("%lf %lf %lf\n", &x, &y, &sound_level);
         	   	sound = storeSounds(sound, x, y,sound_level, index);
          	  	index++;
        	}
        	else{
        	    printf("Stage 1\n");
                printf("==========\n");
                stageFirstScd(sound,ORIGINAL_X,ORIGINAL_Y,index,
                               STAGE_ONE_INDEX);
                check=1;
        	}
        }
        if (stage_id == STAGE_TWO_ID){
            scanf("%lf %lf \n", &x, &y);
            printf("\n");
            printf("Stage 2\n");
            printf("==========\n");
            stageFirstScd(sound,x,y,index,STAGE_TWO_INDEX);
        }
        else if (check==1){
            break;
        }
    }
    printf("\n");
    printf("Stage 3\n");
    printf("==========\n");
    stageThree(sound, index);
    printf("\n");
    printf("Stage 4\n");
    printf("==========\n");
    stageFour(sound, index);
    return 0;
}

/**
 * store sound dats into an array
 * Parameters:
 *      sound(store_t): array to store the data
 *      x_val(double):  X coordinate
 *      y_val(double):  Y coordinate
 *      sound_level(double):  sound level
 *      index(int): the position of the array to store the data
 * Return:
 *      (sound_t) updated array
 */

sound_t **storeSounds(sound_t **sound, double x_val, double y_val,
                      double sound_level, int count){
    sound[count] = (sound_t*)malloc(sizeof(sound_t));
    sound[count]->x = x_val;
    sound[count]->y = y_val;
    sound[count]->sound_level = sound_level;
    return sound;
}
/**
 * Calculate sound level at a given piont
 * invalid input will return 0
 * Parameters:
 *      l2(double): orginal sound level
 *      r1(double): orginal point distance
 *      r2(double): new point distance
 * Return:
 *      (double) sound level at a given piont
 */
double calculateOrDecibel (double l2, double r1, double r2){
	if(l2!=0&&r2!=0){
		double rate = r1/r2;
		return  l2 + 20*log10(rate);
	}
	else{
		return 0;
	}

}
/**
 * Calculate aggregate sound level
 * Parameters:
 *      l2(double): a list stores all sound level
 * Return:
 *      (double) aggregate sound level
 */

double calculateSumDecibel (double *l2){
	double sum=0;
	int i=0;
	for(i=0;i<sizeof(l2);i++){
	   if(l2[i]!=0){
	   	sum=sum+pow(10,(l2[i]/10));
	   }
	}
	  return 10*log10(sum);
}
/**
 * when flag is 1
 * Calculate aggregate sound level contributed by n loudspeakers at orgin
 * when flag is 2
 * Calculate aggregate sound level at giving piont
 * Parameters:
 *      sounds(sound_t): 2-d array with loudspeakers data
 *      x(double): X coordinate
 *      y(double): Y coordinate
 *      index(int): the total number of sound
 * Return:
 *      (double) The aggregate sound level
 */
double stageFirstScd(sound_t **sound,double x, double y,
						int index, int flag){
	//memery allocation for the list
	//the list is used to store the sound level at given piont
	double *list= malloc(index*sizeof(double));
	int i=0;
	for(i=0;i<index;i++){
  		double distance= sqrt(((sound[i]->x-x)*(sound[i]->x-x))
  	                              +((sound[i]->y-y)*(sound[i]->y-y)));

        list[i]=calculateOrDecibel(sound[i]->sound_level,
                                   DEFAULT_DISTANCE,distance);
	}
	//Calculate aggregate sound level
   	double sum = calculateSumDecibel(list);
   	if(sum>=0){
	//for the first stage
        if(flag==STAGE_ONE_INDEX){
            printf("Number of loudspeakers: %02d\n", index);
            printf("Sound level at (000.0, 000.0): %05.2lfdB\n", sum);
        }
        //for the second stage
        else if(flag==STAGE_TWO_INDEX){
            printf("Sound level at (%05.1lf, %05.1lf): %05.2lfdB\n",
                                                        x, y, sum);
        }
        return sum;
    }

    return 0;
}
/**
 * To estimate the overall sound level of a given region (312*312)
 * Parameters:
 *      sound(sound_t): The array that stores WAPs data
 *      index(int): the total number of sound
 * Return:
 *      (Void)
 */
void stageThree(sound_t **sound, int index){
    int row, col;
    int total= 0;
    int threshold = 0;
    for (row = START_ROW; row < MAX_ROW; row=row+EACH_ADD){
        for (col = START_COL; col < MAX_COLUME;col=col+EACH_ADD){
            double sum = stageFirstScd(sound, row, col, index,
                                                STAGE_THREE_INDEX);
            if (sum <= THRESHOLD_ONE){
                threshold++;
            }
            total++;
        }
    }
    printf("%d points sampled\n", total);
    printf("%.4d points (%05.2lf%%) have sound level <= 55dB\n",
            threshold,(double)threshold /total * 100);
}
/**
 * draw a sound map with a grid of characters
 * Parameters:
 *    sound(sound_t): The array that stores WAPs data
 *    index(int): the total number of sound
 * Return:
 *      (Void)
 */
void stageFour(sound_t **sound, int index){
    double row = FOUR_Y_START;
    double col = FOUR_X_START;
    do{
     do{
         double sum = stageFirstScd(sound, col, row, index,
                                           STAGE_FOUR_INDEX);
           if (sum >= THRESHOLD_TWO){
               printf("%c", '+');
            }
           else if (sum >= THRESHOLD_THREE){
               printf("%c", ' ');
            }
           else if (sum >= THRESHOLD_FOUR){
               printf("%c", '8');
            }
           else if (sum >= THRESHOLD_FIVE){
               printf("%c", ' ');
            }
           else if (sum >= THRESHOLD_SIX){
               printf("%c", '6');
            }
           else if (sum > THRESHOLD_ONE){
               printf("%c", ' ');
            }
           else{
               printf("%c", '-');
            }
         col=col+FOUR_WIDTH;
     }while(col<=FOUR_X_MAX);
     row=row-FOUR_HIGHT;
     col = FOUR_X_START;
     printf("\n");
   }while(row>=FOUR_Y_MIN);
}



