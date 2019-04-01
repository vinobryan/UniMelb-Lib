/** ass1.c
 *
 * Created by Jiayin Cai
 * (jiayinc@student.unimelb.edu.au)
 * 08/09/2017
 *
 * To run the program type :
 * ./ass1 query(s) < input_file_name
 *
 * algorithms are fun
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <stdbool.h>

#define STAGE_ONE_PREFIX "S1"
#define STAGE_TWO_PREFIX "S2"
#define STAGE_THREE_PREFIX "S3"
#define STAGE_FOUR_PREFIX "S4"
#define ERROR_EXIT 1
#define LINE_MAX 1000
#define WORD_SPLITER " "
#define MAX_SCORE 5

/**
 * A structure that can store all information
 * about a line in this assignment
 */
typedef struct score_s{
    int line_num;
    double score;
    char *line_char;
    struct score_s *next;
}score_t;

/**
 * Function declaration
 */
void stage_one(int argc, char *argv[]);
score_t* stage_two(int argc, char *argv[]);
double stage_three(int argc, char *argv[], char *line, int char_num);
void stage_four(score_t *root);
score_t* add_score_sorted(score_t *root, double score,
                          int line_num, char *line);
int num_prefix_match(char *query, char *line);
bool is_prefix_match(char *query, char *word);
void print_first_five(score_t *root);
void malloc_fail(void);
void pure_word(char* raw_word);

/**
 * Main function
 */
int main(int argc, char *argv[]){
    score_t *root;
	stage_one(argc, argv);
	root = stage_two(argc, argv);
	stage_four(root);
	return 0;
}

/**
 * Print each query from user input
 * Parameters:
 *      argc(int): the number of queries
 *      argv(int): the content of queries
 * Return:
 *      Void
 */
void print_query(int argc, char *argv[]){
    int query;
    printf("%s: query = ", STAGE_ONE_PREFIX);
    for (query=1; query<argc; query++){
        printf("%s ", argv[query]);
    }
    printf("\n");
}

/**
 * Actions after memory allocation fails
 * Parameters:
 *      Void
 * Return:
 *      Void
 */
void malloc_fail(void){
    printf("malloc error\n");
    fflush(stdout);
    exit(ERROR_EXIT) ;
}

/**
 * Print the message at the end of each
 * answer
 * Parameters:
 *      Void
 * Return:
 *      Void
 */
void print_answer_end(void){
    printf("---\n");
}

/**
 * The stage one of the assignment
 * Print the query from the command-line
 * Parameters:
 *      argc(int): the number of queries
 *      argv(int): the content of queries
 * Return:
 *      Void
 */
void stage_one(int argc, char *argv[]){
	if (argc <= 1){
		printf("%s: No query specified, must provide at least one word\n",
               STAGE_ONE_PREFIX);
		exit(ERROR_EXIT);
	}else{
	    int array, query;
        print_query(argc, argv);
	    for (query=1; query<argc; query++){
            for (array=0; array<strlen(argv[query]); array++){
                if (isupper(argv[query][array])){
                    printf("%s: invalid character(s) in query\n", argv[query]);
                    break;
                }
            }
	    }
	}
}

/**
 * Stage two & three of the assignment
 * Print out each input line and it's score
 * Parameters:
 *      argc(int): the number of queries
 *      argv(int): the content of queries
 * Return:
 *      score_t: The information of all non-zero score line
 */
score_t* stage_two(int argc, char *argv[]){
    char *line;
    char temp_char;
    score_t *root_store = NULL;
    bool in_word = false;
    int char_count = 0, line_count = 1, word_count = 0;
    if ((line = (char*)malloc(LINE_MAX*sizeof(char))) == NULL) {
        malloc_fail();
    }
    while ((temp_char = getchar()) != EOF){
        if (temp_char != '\n'){
            if(isalnum(temp_char)){
                in_word = true;
            }else if(in_word){
                word_count++;
                in_word = false;
            }
            line[char_count] = temp_char;
            char_count++;
        }else{
            double score;
            line[char_count] = '\0';
            if(isalnum(line[char_count-1]))
                word_count++;
            char_count--;
            if(char_count!=0){
                print_answer_end();
                printf("%s\n", line);
                printf("%s: line = %d, bytes = %d, words = %d\n",
                        STAGE_TWO_PREFIX, line_count, char_count, word_count);
                score = stage_three(argc, argv, line, word_count);
                printf("%s: line = %d, score = %.3lf\n",
                        STAGE_THREE_PREFIX, line_count, score);
                if (score!=0){
                    root_store = add_score_sorted(root_store, score,
                                                  line_count, line);
                }
            }
            line_count++;
            word_count = 0;
            char_count = 0;
        }
    }
    return root_store;
}

/**
 * Stage three of the assignment
 * Calculate the score of the line
 * Parameters:
 *      argc(int): the number of queries
 *      argv(int): the content of queries
 *      line(char *): the content of a line
 *      word_num(int): the number of words in the line
 * Return:
 *      double: the score of the line
 */
double stage_three(int argc, char *argv[], char *line, int word_num){
    int index;
    double sum = 0.0, bottom = log(8.5 + word_num)/log(2.0);
    for (index=1; index<argc; index++){
        int matches = num_prefix_match(argv[index], line);
        sum = sum + log(1 + (double) matches)/log(2.0);
    }
    return sum/bottom;
}

/**
 * Stage four of the assignment
 * Print score lines in decreasing score order
 * Parameters:
 *      argc(int): the number of queries
 *      argv(int): the content of queries
 * Return:
 *      Void
 */
void stage_four(score_t *root){
    printf("------------------------------------------------\n");
    print_first_five(root);

}

/**
 * Print top 5 score lines in the list
 * Parameters:
 *      root(score_t *): the root of the list
 * Return:
 *      Void
 */
void print_first_five(score_t *root){
    score_t *current = root;
    int count = 0;
    while (current!=NULL && count<MAX_SCORE){
        printf("%s: line = %d, score = %.3lf\n",
                STAGE_FOUR_PREFIX, current->line_num, current->score);
        printf("%s\n", current->line_char);
        print_answer_end();
        current = current->next;
        count++;
    }
    printf("\n");
}

/**
 * Add score line into a sorted list
 * Parameters:
 *      root(score_t *): the root of the list
 *      score(double): the score of that line
 *      line_num(int): the number of that line
 *      line(char *): the content of that line
 * Return:
 *      score_t*: a list with the new score line
 */
score_t* add_score_sorted(score_t *root, double score,
                          int line_num, char *line){
    score_t *current = root, *previous = NULL;
    score_t *new_score;
    int retain_num = 0;
    while ((current != NULL) && (current->score >= score)) {
        previous = current;
		current = current->next;
		retain_num++;
	}
	if (retain_num>=MAX_SCORE){
        return root;
    }
    if ((new_score = (score_t*)malloc(sizeof(score_t))) == NULL) {
        malloc_fail();
    }
    if ((new_score->line_char =
        (char*)malloc(LINE_MAX*sizeof(char))) == NULL) {
        malloc_fail();
    }
    strcpy(new_score->line_char, line);
    new_score->score = score;
    new_score->line_num = line_num;
    new_score->next = current;
    if (previous == NULL) {
	    root = new_score;
	}else{
        previous->next = new_score;
	}
	return root;
}


/**
 * Count number of prefix matches in a line using a
 * specific query
 * Parameters:
 *      query(char *): the query that needs to do comparing
 *      line(char *): the line that needs to do comparing
 * Return:
 *      int: the number of matches
 */
int num_prefix_match(char *query, char *line){
    int match_count = 0;
    char *temp_line;
    char *word;
    if ((temp_line = (char*)malloc(LINE_MAX*sizeof(char))) == NULL) {
        malloc_fail();
    }
    strcpy(temp_line, line);
    word = strtok(temp_line, WORD_SPLITER);
    while(word){
        pure_word(word);
        if (is_prefix_match(query, word)){
            match_count++;
        }
        word = strtok(NULL, WORD_SPLITER);
    }
    free(temp_line);
    return match_count;
}

/**
 * Remove the non-alpha and non-number characters
 * in a string
 * Parameters:
 *      raw_word(char *): the word needed to be purified
 * Return:
 *      Void
 */
void pure_word(char* raw_word){
    char *word;
    int index = 0, raw_word_index = 0;
    if ((word = (char*)malloc(LINE_MAX*sizeof(char))) == NULL) {
        malloc_fail();
    }
    while (raw_word_index < strlen(raw_word)){
        if (isalnum(raw_word[raw_word_index])){
            word[index] = raw_word[raw_word_index];
            index++;
        }
        raw_word_index++;
    }
    word[index] = '\0';
    strcpy(raw_word, word);
    free(word);
}

/**
 * Whether the prefix matches the word
 * Parameters:
 *      query(char *): the query that needs to do comparing
 *      word(char *): the word that needs to do comparing
 * Return:
 *      bool: true if the prefix matches the word
 */
bool is_prefix_match(char *query, char *word){
    int word_num;
    int match_count = 0;
    for (word_num=0; word_num<strlen(query); word_num++){
        if (tolower(word[word_num])==tolower(query[word_num])){
            match_count++;
        }
    }
    if (match_count==strlen(query)){
        return true;
    }
    return false;
}
