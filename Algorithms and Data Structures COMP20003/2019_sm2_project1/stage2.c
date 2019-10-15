/** stage2.c
 *
 * Created by Jiayin Cai
 *
 * Input a csv file and a key (or keys), then construct a binary
 * tree and output searching result of pu_locationID in that file.
 *
 * To build this stage, type:
 * make dict2
 *
 * To run the program type :
 * ./dict2 input_file output_file < keys_file
 * Or
 * ./dict2 input_file output_file, then type keys
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
    int cmp = 0;
    if (root!=NULL){
        cmp = cmp + search_tree(root->left, key, file, found);
        if (strcmp(key, root->data->pu_locationID) == 0){
            write_all_duplicates(root, file);
            *found = true;
        }
        cmp = cmp + search_tree(root->right, key, file, found);
        return cmp + 1;
    }else{
        return cmp;
    }
}

/**
 * Appending a string into a file
 * Parameters:
 *       char data: The data that to be output
 *       char file_name: The output file name
 */
void write_search_result(char *key, data_t *data, char *file_name){
    FILE *f = fopen(file_name, "a+");
    if (f == FILE_OPEN_FAIL){
        printf("can not access the file.\n");
    }
    fprintf(f, "%s --> %s\n", key, data->pu_datetime);
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
        write_search_result(current->pu_locationID, current, file);
        current = current->next;
    }
}