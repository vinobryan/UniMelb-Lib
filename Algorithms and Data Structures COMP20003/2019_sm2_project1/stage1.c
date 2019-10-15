/** stage1.c
 *
 * Created by Jiayin Cai
 *
 * Input a csv file and a key (or keys), then construct a binary
 * tree and output searching result of pu_datetime in that file.
 *
 * To build this stage, type:
 * make dict1
 *
 * To run the program type :
 * ./dict1 input_file output_file < keys_file
 * Or
 * ./dict1 input_file output_file, then type keys
 *
 */
#include "header.h"

int main(int argc, char *argv[]){
    tree_t *root = NULL;
    char *input_file = argv[1];
    char *output_file = argv[2];
    root = construct_tree(input_file, root);
    read_keys(root, output_file);
    return 0;
}

/**
 * Search a binary tree
 * Parameters:
 *      tree_t root: the root of a tree
 *      char key: the search key
 *      char file: the output file name
 *      bool found: whether the key has found or not
 * Return:
 *      int: comparison number
 */
int search_tree(tree_t *root, char *key, char *file, bool *found){
    if (root!=NULL){
        if (strcmp(key, root->data->pu_datetime) < 0){
            return search_tree(root->left, key, file, found) + 1;
        }else if(strcmp(key, root->data->pu_datetime) > 0){
            return search_tree(root->right, key, file, found) + 1;
        }else{
            //write_search_result(key, root->data, file);
            write_all_duplicates(root, file);
            *found = true;
            return 1;
        }
    }else{
        if (!found){
            write_not_found(key, file);
        }
        return 0;
    }
}

/**
 * Appending a string into a file
 * Parameters:
 * char data: The data that to be output
 * char file_name: The output file name
 */
void write_search_result(char *key, data_t *data, char *file_name){
    FILE *f = fopen(file_name, "a+");
    if (f == FILE_OPEN_FAIL){
        printf("can not access the file.\n");
    }
    fprintf(f, "%s --> VendorID: %s || ", key, data->id);
    fprintf(f, "passenger_count: %s || ", data->passenger_count);
    fprintf(f, "trip_distance: %s || ", data->trip_distance);
    fprintf(f, "RatecodeID: %s || ", data->ratecode_id);
    fprintf(f, "store_and_fwd_flag: %s || ", data->store_and_fwd_flag);
    fprintf(f, "PULocationID: %s || ", data->pu_locationID);
    fprintf(f, "DOLocationID: %s || ", data->do_locationID);
    fprintf(f, "payment_type: %s || ", data->payment_type);
    fprintf(f, "fare_amount: %s || ", data->fare_amount);
    fprintf(f, "extra: %s || ", data->extra);
    fprintf(f, "mta_tax: %s || ", data->mta_tax);
    fprintf(f, "tip_amount: %s || ", data->tip_amount);
    fprintf(f, "tolls_amount: %s || ", data->tolls_amount);
    fprintf(f, "improvement_surcharge: %s || ", data->improvement_surcharge);
    fprintf(f, "total_amount: %s || ", data->total_amount);
    fprintf(f, "DOdatetime: %s || ", data->do_datetime);
    fprintf(f, "trip_duration: %s ||\n", data->trip_duration);
    fclose(f);
}

/**
 * Traversal all results in the linked-list
 * Parameters:
 *      tree_t root: the root of a tree
 *      char file: the output file name
 * Return:
 *      void
 */
void write_all_duplicates(tree_t *root, char *file){
    data_t *current = root->data;
    while (current!=NULL){
        write_search_result(current->pu_datetime, current, file);
        current = current->next;
    }
}

