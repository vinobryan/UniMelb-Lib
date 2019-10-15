/** assmt2.c
 *
 * Created by *****
 * (****@student.unimelb.edu.au)
 * 19/05/2017
 *
 * To run the program type :
 * ./assmt2 < input_file
 * Or
 * ./assmt2, then type data
 *
 * algorithms are fun
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <stdbool.h>

#define STAGE_1 1
#define STAGE_2 2
#define STAGE_3 3
#define STAGE_4 4
#define MID_POINT 2
#define MAX_SIZE_WORD 30
#define MAX_SIZE_NAME 30
#define MAX_NAMES 100
#define NUM_PERCENT_SYMBOL 10
#define INPUT_CHAR_ERROR -1
#define EXIT_CODE 1
#define EQUAL 0
#define STRING_INIT ""
#define CHAR_INIT ' '
#define INT_START 0
#define NOT_FOUND -1
#define FIRST_NAME "FIRST_NAME"
#define LAST_NAME "LAST_NAME"
#define NOT_NAME "NOT_NAME"

typedef struct name_dict_s{
    char name[MAX_SIZE_NAME];
    int first;
    int last;
    int non_name;
}name_dict_t;

typedef struct word_list_s{
    char *word;
    struct word_list_s *next;
}word_list_t;

void print_stage_header(int stage);
void malloc_fail();
void stage_one(name_dict_t *data);
void stage_two(name_dict_t *data, int num_name);
void stage_three(word_list_t *root);
void stage_four(word_list_t *root,
                name_dict_t name_data[], int num_name);
word_list_t *read_scentence(word_list_t *root);
word_list_t *add_to_list(word_list_t *root, char *name);
int read_names(name_dict_t data[]);
int b_search(char *key, name_dict_t name_data[], int start, int end);

/**
 * Main function
 */
int main(int argc, char *argv[]){
    int num_name;
    word_list_t *root = NULL;
    name_dict_t dict[MAX_NAMES];

    num_name = read_names(dict);
    root = read_scentence(root);
    stage_one(dict);
    stage_two(dict, num_name);
    stage_three(root);
    stage_four(root, dict, num_name);
}

/**
 * Print the header of each stage
 * Parameters:
 *      stage(int): The stage that you need to print the header
 * Return:
 *      Void
 */
void print_stage_header(int stage){
    printf("=========================");
    printf("Stage %d", stage);
    printf("=========================\n");
}

/**
 * Read the list of ordered names
 * Parameters:
 *      data (name_dict_t[]): The array of dictionary
 * Return:
 *      int: Number of names read
 */
int read_names(name_dict_t data[]){
    int index = INT_START;
    while (index < MAX_NAMES){
        char id = getchar();
        if (id == '#'){
            /** Read data of names */
            scanf("%s\n", data[index].name);
            scanf("%d %d %d\n",
                  &data[index].first, &data[index].last, &data[index].non_name);
        }else if (id == '%'){
            /** if end of name reading */
            char percent_symbol[NUM_PERCENT_SYMBOL];
            scanf("%s\n", percent_symbol);
            if (strlen(percent_symbol)==NUM_PERCENT_SYMBOL-1){
                /** The first one has been read using get char */
                break;
            }
        }
        index++;
    }
    return index;
}

/**
 * Add item to word list
 * Parameters:
 *      root (word_list_t *): The root of linked list
 *      name (char []): The data to be added
 * Return:
 *      word_list_t *: The root of that list after adding
 */
word_list_t *add_to_list(word_list_t *root, char name[]){
    if (root!=NULL){
        root->next = add_to_list(root->next, name);
    }else{
        if ((root = (word_list_t*)malloc(sizeof(word_list_t*))) == NULL) {
            malloc_fail();
        }
        if ((root->word =
             (char*)malloc(MAX_SIZE_WORD * sizeof(char*))) == NULL) {
            malloc_fail();
        }
        strcpy(root->word, name);
        root->next = NULL;
    }
    return root;
}

/**
 * Read words into linked list
 * Parameters:
 *      root (word_list_t *): The root of linked list
 * Return:
 *      word_list_t *: The root of that list after reading
 */
word_list_t *read_scentence(word_list_t *root){
    char single_char = CHAR_INIT;
    while (single_char!=INPUT_CHAR_ERROR){
        char word[MAX_SIZE_WORD] = STRING_INIT;
        int index = INT_START;
        while (true){
            single_char=getchar();
            if ((single_char!=' ')&&(single_char!=INPUT_CHAR_ERROR)){
                word[index] = single_char;
                index++;
            }else{
                break;
            }
        }

        root = add_to_list(root, word);
    }
    return root;
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
    exit(EXIT_CODE) ;
}

/**
 * Stage one, read in names from the input
 * data, and output them
 * Parameters:
 *      data (name_dict_t[]): The array of dictionary
 *      num_name (int): Number of names in array
 * Return:
 *      Void
 */
void stage_one(name_dict_t *data){
    int index=INT_START;
    print_stage_header(STAGE_1);
    printf("Word %d: %s\n", index, data[index].name);
    printf("Label probabilities: %d%% %d%% %d%%\n",
            data[index].first, data[index].last, data[index].non_name);
    printf("\n");
}

/**
 * Stage two, output the summary of the dictionary
 * Parameters:
 *      root (word_list_t *): The root of linked list
 * Return:
 *      Void
 */
void stage_two(name_dict_t *data, int num_name){
    int index;
    int sum_of_size = INT_START;
    print_stage_header(STAGE_2);
    for (index=INT_START; index<num_name; index++){
        sum_of_size = sum_of_size + strlen(data[index].name);
    }
    printf("Number of words: %d\n", num_name);
    printf("Average number of characters per word: %.2f\n",
           (double)sum_of_size/num_name);
    printf("\n");
}

/**
 * Stage three, output the linked list
 * Parameters:
 *      root (word_list_t *): The root of linked list
 * Return:
 *      Void
 */
void stage_three(word_list_t *root){
    print_stage_header(STAGE_3);
    while (root!=NULL){
        printf("%s\n", root->word);
        root = root->next;
    }
    printf("\n");
}

/**
 * Stage four, output the linked list with comparing
 * Parameters:
 *      root (word_list_t *): The root of linked list
 *      name_data (name_dict_t[]): The array of dictionary
 *      num_name (int): Number of names in array
 * Return:
 *      Void
 */
void stage_four(word_list_t *root, name_dict_t name_data[], int num_name){
    print_stage_header(STAGE_4);
    while (root!=NULL){
        int mid = b_search(root->word, name_data, INT_START, num_name);
        printf("%-32s", root->word);
        if (mid>=EQUAL){
            if (name_data[mid].first!=EQUAL){
                printf("%s", FIRST_NAME);
                if (name_data[mid].last!=EQUAL)
                    printf(", %s", LAST_NAME);
            }else if (name_data[mid].last!=EQUAL)
                    printf("%s", LAST_NAME);
            printf("\n");
        }else{
            printf("%s\n", NOT_NAME);
        }
        root = root->next;
    }
    printf("\n");
}

/**
 * Binary search algorithm
 * Parameters:
 *      key (char *): The comparing key
 *      name_data (name_dict_t[]): The array of dictionary
 *      start (int): Start point of searching
 *      end (int): End point of searching
 * Return:
 *      Void
 */
int b_search(char *key, name_dict_t name_data[], int start, int end){
    int mid = start + (end-start)/MID_POINT;
    if (start>end){
        return NOT_FOUND;
    }
    if (strcmp(key, name_data[mid].name)==EQUAL){
        return mid;
    }
    if (strcmp(key, name_data[mid].name)<EQUAL){
        return b_search(key, name_data, start, mid-1);
    }
    if (strcmp(key, name_data[mid].name)>EQUAL){
        return b_search(key, name_data, mid+1, end);
    }
    return NOT_FOUND;
}
