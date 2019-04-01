/** stage1.c
 *
 * Created by Jiayin Cai
 *
 * Input a csv file and a key (or keys), then construct a binary
 * tree and output searching result of the key in that file.
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
    char *input_data = argv[1];
    char *output_file = argv[2];
    root = construct_tree(input_data, root);
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
int search_tree(tree_t *root, char *key, char *file, bool found){
    if (root!=NULL){
        if (strcmp(key, root->name) < 0){
            return search_tree(root->left, key, file, found) + 1;
        }else if(strcmp(key, root->name) > 0){
            return search_tree(root->right, key, file, found) + 1;
        }else{
            write_search_result(key, root->data, file);
            return search_tree(root->left, key, file, true) + 1;
        }
    }else{
        if (!found){
            write_not_found(key, file);
        }
        return 0;
    }
}

/**
 * Add an item into the tree
 * Parameters:
 *  ID - Unique number for each athlete
    Name - Athlete¡¯s name
    Sex - M or F
    Age - Integer
    Height - In centimeters
    Weight - In kilograms
    Team - Team name
    NOC - National Olympic Committee 3-letter code
    Games - Year and season
    Year - Integer
    Season - Summer or Winter
    City - Host city
    Sport - Sport
    Event - Event
    Medal - Gold, Silver, Bronze, or NA
 * Return:
 *      tree_t: a tree containing new item
 */
tree_t *add_to_tree(tree_t *root, char *id, char *name, char *sex,
                    char *age, char *height, char *weight, char *team,
                    char *noc, char *game, char *year, char *season,
                    char *city, char *sport, char *event, char *medal){
    if (root!=NULL){
        if (strcmp(name, root->name) <= 0){
            root->left = add_to_tree(root->left, id, name, sex, age, height,
                                     weight, team, noc, game, year, season,
                                     city, sport, event, medal);
        }else{
            root->right = add_to_tree(root->right, id, name, sex, age,
                                      height, weight, team, noc, game,
                                      year, season, city, sport, event,
                                      medal);
        }

    }else{
        if ((root = (tree_t*)malloc(sizeof(tree_t))) == NULL) {
            malloc_fail();
        }
        if ((root->data = (data_t*)malloc(sizeof(data_t))) == NULL) {
            malloc_fail();
        }
        strcpy(root->name, name);
        strcpy(root->data->id, id);
        strcpy(root->data->sex, sex);
        strcpy(root->data->age, age);
        strcpy(root->data->height, height);
        strcpy(root->data->weight, weight);
        strcpy(root->data->team, team);
        strcpy(root->data->noc, noc);
        strcpy(root->data->game, game);
        strcpy(root->data->year, year);
        strcpy(root->data->season, season);
        strcpy(root->data->city, city);
        strcpy(root->data->sport, sport);
        strcpy(root->data->event, event);
        strcpy(root->data->medal, medal);
        root->right = NULL;
        root->left = NULL;
    }
    return root;
}
