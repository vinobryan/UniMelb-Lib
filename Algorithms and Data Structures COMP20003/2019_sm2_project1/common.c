/** common.c
 *
 * Created by Jiayin Cai
 *
 * Common functions in stage1 and stage2
 *
 */

#include "header.h"

/**
 * Construct a binary search tree from a file
 * Parameters:
 * char *datafile: The file name of input file
 * tree_t *root: The root of the binary search tree
 */
tree_t *construct_tree(char *datafile, tree_t *root){
    FILE *fp = NULL;
    char line[MAX_LEN_LINE];
    if((fp = fopen(datafile, "at+")) != NULL){
        while (fgets(line, MAX_LEN_LINE, fp)!=NULL){
            root = add_data(root, line);
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
 *      tree_t *root: the root of the list
 *      char *line: one line of data
 * Return:
 *      tree_t: a list containing new items
 */
tree_t *add_data(tree_t *root, char *line){
    data_t *data;
    if ((data = (data_t*)malloc(sizeof(data_t))) == NULL) {
        malloc_fail();
    }
    strcpy(data->id, strtok(line, SEMICONLON));
    strcpy(data->passenger_count, strtok(NULL, SEMICONLON));
    strcpy(data->trip_distance, strtok(NULL, SEMICONLON));
    strcpy(data->ratecode_id, strtok(NULL, SEMICONLON));
    strcpy(data->store_and_fwd_flag, strtok(NULL, SEMICONLON));
    strcpy(data->pu_locationID, strtok(NULL, SEMICONLON));
    strcpy(data->do_locationID, strtok(NULL, SEMICONLON));
    strcpy(data->payment_type, strtok(NULL, SEMICONLON));
    strcpy(data->fare_amount, strtok(NULL, SEMICONLON));
    strcpy(data->extra, strtok(NULL, SEMICONLON));
    strcpy(data->mta_tax, strtok(NULL, SEMICONLON));
    strcpy(data->tip_amount, strtok(NULL, SEMICONLON));
    strcpy(data->tolls_amount, strtok(NULL, SEMICONLON));
    strcpy(data->improvement_surcharge, strtok(NULL, SEMICONLON));
    strcpy(data->total_amount, strtok(NULL, SEMICONLON));
    strcpy(data->pu_datetime, strtok(NULL, SEMICONLON));
    strcpy(data->do_datetime, strtok(NULL, SEMICONLON));
    
    // remove new line char
    char *last_var = strtok(NULL, SEMICONLON);
    strcpy(data->trip_duration, strtok(last_var, "\n"));

    root = add_to_binary_search_tree(root, data);
    return root;
}

/**
 * Add an item into the tree
 * Parameters:
 *      tree_t *root: root of the tree
 *      data_t *data: data to be inserted
 * Return:
 *      tree_t: a tree containing new item
 */
tree_t *add_to_binary_search_tree(tree_t *root, data_t *data){
    if (root!=NULL){
        int cmp_result = strcmp(data->pu_datetime, root->data->pu_datetime);
        if (cmp_result < 0){
            root->left = add_to_binary_search_tree(root->left, data);
        }else if (cmp_result > 0){
            root->right = add_to_binary_search_tree(root->right, data);
        }else{
            root->data = save_data(root->data, data);
        }

    }else{
        if ((root = (tree_t*)malloc(sizeof(tree_t))) == NULL) {
            malloc_fail();
        }
        root->data = NULL;
        root->data = save_data(root->data, data);
        root->right = NULL;
        root->left = NULL;
    }
    return root;
}

/**
 * Save data into a linked-list
 * Parameters:
 * data_t *current: the linked-list of data
 * data_t *data: data need to be inserted
 */
data_t *save_data(data_t *current, data_t *data){
    if (current!=NULL){
        current->next = save_data(current->next, data);

    }else{
        current = data;
        current->next = NULL;
    }
    return current;
}

/**
 * Output not found result into a file
 * Parameters:
 * char key: the search key
 * char file: the output file name
 */
void write_not_found(char *key, char *file_name){
    FILE *f = fopen(file_name, "a+");
    if (f == FILE_OPEN_FAIL){
        printf("can not access the file.\n");
    }
    fprintf(f, "%s --> NOTFOUND\n ", key);
    fclose(f);
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

/**
 * Read searching keys from stdin or file
 * Parameters:
 * tree_t *root: The root of a tree
 * char output_file: The name of output file
 */
void read_keys(tree_t *root, char *output_file){
    char key[MAX_LEN_STR];
    while (fgets(key, MAX_LEN_STR, stdin) != NULL){
        key[strcspn(key, "\n")] = '\0';
        bool found = false;
        int comparison = search_tree(root, key, output_file, &found);
        printf("%s --> %d\n", key, comparison);
    }
}


