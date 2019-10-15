#ifndef HEADERH
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LEN_STR 128
#define MAX_LEN_LINE 512
#define EXIT_FAILURE 1
#define SEMICONLON ","
#define FILE_OPEN_FAIL 0
#define WRITE_FILE_MOVE 0

typedef struct data_s{
    char id[MAX_LEN_STR];
    char sex[MAX_LEN_STR];
    char age[MAX_LEN_STR];
    char height[MAX_LEN_STR];
    char weight[MAX_LEN_STR];
    char team[MAX_LEN_STR];
    char noc[MAX_LEN_STR];
    char game[MAX_LEN_STR];
    char year[MAX_LEN_STR];
    char season[MAX_LEN_STR];
    char city[MAX_LEN_STR];
    char sport[MAX_LEN_STR];
    char event[MAX_LEN_STR];
    char medal[MAX_LEN_STR];
    struct data_s *next;
}data_t;

typedef struct tree_s{
    char name[MAX_LEN_STR];
    data_t *data;
    struct tree_s *left;
    struct tree_s *right;
}tree_t;

tree_t *read_all(tree_t *root);
tree_t *add_to_tree(tree_t *root, char *id, char *name, char *sex, char *age,
                    char *height, char *weight, char *team, char *noc, char *game,
                    char *year, char *season, char *city, char *sport, char *event,
                    char *medal);
tree_t *build_a_leaf(tree_t *root, char *id, char *name, char *sex,
                     char *age, char *height, char *weight, char *team,
                     char *noc, char *game, char *year, char *season,
                     char *city, char *sport, char *event, char *medal);
data_t *save_data(data_t *current, char *id, char *name, char *sex, char *age,
                char *height, char *weight, char *team, char *noc, char *game,
                char *year, char *season, char *city, char *sport, char *event,
                char *medal);
tree_t *reformat_and_add_to_tree(tree_t *root, char *line);
tree_t *construct_tree(char datafile[], tree_t *root);
int search_tree(tree_t *root, char *key, char *file, bool found);
void write_search_result(char *key, data_t *data, char *file_name);
void write_to_file(char *data, char *file_name);
void write_not_found(char *key, char *file);
void write_all_duplicates(tree_t *root, char *file);
void read_keys(tree_t *root, char *output_file);
void malloc_fail(void);

#endif
