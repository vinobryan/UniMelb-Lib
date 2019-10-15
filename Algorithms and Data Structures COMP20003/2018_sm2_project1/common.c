/** common.c
 *
 * Created by Jiayin Cai
 *
 * Common functions in stage1 and stage2
 *
 */

#include "header.h"

/**
 * Output not found result into a file
 * Parameters:
 * char key: the search key
 * char file: the output file name
 */
void write_not_found(char *key, char *file){
    char line[MAX_LEN_LINE];
    sprintf(line, "%s --> NOTFOUND\n", key);
    write_to_file(line, file);
}

/**
 * Output seach result into a file
 * Parameters:
 * char key: the search key
 * data_t data: the output data
 * char file_name: the output file name
 */
void write_search_result(char *key, data_t *data, char *file_name){
    char line[MAX_LEN_LINE];
    sprintf(line, "%s --> ID:%s Sex:%s || Age:%s || Height:%s"\
            " || Weight:%s || Team:%s"\
            " || NOC:%s || Games:%s || Year:%s || Season:%s || "\
            "City:%s || Sport:%s ||"\
            "Event:%s || Medal:%s\n",
            key, data->id, data->sex, data->age,
            data->height, data->weight, data->team,
            data->noc, data->game, data->year,
            data->season, data->city, data->sport,
            data->event, data->medal);
    write_to_file(line, file_name);
}

/**
 * Output data into a file, adding data at the end of file
 * Parameters:
 * char data: The data that to be output
 * char file_name: The output file name
 */
void write_to_file(char *data, char *file_name){
    FILE *fp = fopen(file_name, "a+");
    if (fp == FILE_OPEN_FAIL){
        printf("can't open file\n");
    }
    fprintf(fp, data);
    fclose(fp);
}

/**
 * Read searching keys from user input or file
 * Parameters:
 * tree_t *root: The main tree
 * char output_file: The name of output file
 */
void read_keys(tree_t *root, char *output_file){
    char key[MAX_LEN_STR];
    while (fgets(key, MAX_LEN_STR, stdin) != NULL){
        key[strcspn(key, "\n")] = '\0';
        int comparison = search_tree(root, key, output_file, false);
        printf("%s --> %d\n", key, comparison);
    }
}

/**
 * Construct the tree using given data from a file
 * Parameters:
 * char datafile[]: The file name of input file
 */
tree_t *construct_tree(char *datafile, tree_t *root){
    FILE *fp = NULL;
    char line[MAX_LEN_LINE];
    if((fp = fopen(datafile, "at+")) != NULL){
        while (fgets(line, MAX_LEN_LINE, fp)!=NULL){
            root = reformat_and_add_to_tree(root, line);
        }
        fclose(fp);
        fp = NULL;
    }
    return root;
}

/**
 * Read all information from input and
 * stores into a list
 * Parameters:
 *      root(tree_t): the root of the list
 * Return:
 *      tree_t: a list containing new items
 */
tree_t *reformat_and_add_to_tree(tree_t *root, char *line){
    char *id, *age, *height, *weight, *year;
    char *name, *sex, *team, *game;
    char *noc, *season, *city;
    char *sport, *event, *medal;
    id = strtok(line, SEMICONLON);
    name = strtok(NULL, SEMICONLON);
    sex = strtok(NULL, SEMICONLON);
    age = strtok(NULL, SEMICONLON);
    height = strtok(NULL, SEMICONLON);
    weight = strtok(NULL, SEMICONLON);
    team = strtok(NULL, SEMICONLON);
    noc = strtok(NULL, SEMICONLON);
    game = strtok(NULL, SEMICONLON);
    year = strtok(NULL, SEMICONLON);
    season = strtok(NULL, SEMICONLON);
    city = strtok(NULL, SEMICONLON);
    sport = strtok(NULL, SEMICONLON);
    event = strtok(NULL, SEMICONLON);
    medal = strtok(NULL, SEMICONLON);
    root = add_to_tree(root, id, name, sex, age, height, weight,
                       team, noc, game, year, season, city,
                       sport, event, medal);
    return root;
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
