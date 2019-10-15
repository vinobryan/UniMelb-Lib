/** stage2.c
 *
 * Created by Jiayin Cai
 *
 * Input a csv file and a key (or keys), then construct a binary
 * tree and output searching result of the key in that file.
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
            write_all_duplicates(root, file);
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
 * Search all duplicates in the list
 * Parameters:
 *      tree_t root: the root of a tree
 *      char file: the output file name
 * Return:
 *      void
 */
void write_all_duplicates(tree_t *root, char *file){
    data_t *current = root->data;
    while (current!=NULL){
        write_search_result(root->name, current, file);
        current = current->next;
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
                    char *city, char *sport, char *event,
                    char *medal){
    if (root!=NULL){
        if (strcmp(name, root->name) < 0){
            root->left = add_to_tree(root->left, id, name, sex, age, height,
                                     weight, team, noc, game, year, season,
                                     city, sport, event, medal);
        }else if(strcmp(name, root->name) > 0){
            root->right = add_to_tree(root->right, id, name, sex, age,
                                      height, weight, team, noc, game,
                                      year, season, city, sport,
                                      event, medal);
        }else{
            root = build_a_leaf(root, id, name, sex, age, height,
                            weight, team, noc, game, year, season,
                            city, sport, event, medal);
        }

    }else{
        root = build_a_leaf(root, id, name, sex, age, height,
                            weight, team, noc, game, year, season,
                            city, sport, event, medal);
    }
    return root;
}

/**
 * Build a tree leaf
 * Parameters:
 *      data
 * Return:
 *      tree_t: the current leaf of tree
 */
tree_t *build_a_leaf(tree_t *root, char *id, char *name, char *sex,
                     char *age, char *height, char *weight,
                     char *team, char *noc, char *game, char *year,
                     char *season, char *city, char *sport, char *event,
                     char *medal){
    if (root==NULL){
        if ((root = (tree_t*)malloc(sizeof(tree_t))) == NULL) {
            malloc_fail();
        }
        strcpy(root->name, name);
        root->right = NULL;
        root->left = NULL;
        root->data = NULL;
    }
    root->data = save_data(root->data, id, name, sex, age, height,
              weight, team, noc, game, year, season,
              city, sport, event, medal);

    return root;
}

/**
 * Save data into a tree leaf
 * Parameters:
 *      data
 * Return:
 *      data_t: the data of the leaf
 */
data_t *save_data(data_t *current, char *id, char *name, char *sex,
                     char *age, char *height, char *weight,
                     char *team, char *noc, char *game, char *year,
                     char *season, char *city, char *sport, char *event,
                     char *medal){
    if (current!=NULL){
        current->next = save_data(current->next, id, name, sex, age, height,
                            weight, team, noc, game, year, season,
                            city, sport, event, medal);

    }else{
        if ((current = (data_t*)malloc(sizeof(data_t))) == NULL) {
            malloc_fail();
        }
        strcpy(current->id, id);
        strcpy(current->sex, sex);
        strcpy(current->age, age);
        strcpy(current->height, height);
        strcpy(current->weight, weight);
        strcpy(current->team, team);
        strcpy(current->noc, noc);
        strcpy(current->game, game);
        strcpy(current->year, year);
        strcpy(current->season, season);
        strcpy(current->city, city);
        strcpy(current->sport, sport);
        strcpy(current->event, event);
        strcpy(current->medal, medal);
        current->next = NULL;
    }
    return current;
}
