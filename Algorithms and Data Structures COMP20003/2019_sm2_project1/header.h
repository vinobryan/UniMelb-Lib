#ifndef HEADERH
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LEN_STR 128
#define MAX_LEN_LINE 256
#define MAX_LEN_OUTPUT_LINE 2469
#define EXIT_FAILURE 1
#define SEMICONLON ","
#define FILE_OPEN_FAIL 0
#define WRITE_FILE_MOVE 0

typedef struct data_s{
    char id[MAX_LEN_STR];
    char passenger_count[MAX_LEN_STR];
    char trip_distance[MAX_LEN_STR];
    char ratecode_id[MAX_LEN_STR];
    char store_and_fwd_flag[MAX_LEN_STR];
    char pu_locationID[MAX_LEN_STR];
    char do_locationID[MAX_LEN_STR];
    char payment_type[MAX_LEN_STR];
    char fare_amount[MAX_LEN_STR];
    char extra[MAX_LEN_STR];
    char mta_tax[MAX_LEN_STR];
    char tip_amount[MAX_LEN_STR];
    char tolls_amount[MAX_LEN_STR];
    char improvement_surcharge[MAX_LEN_STR];
    char total_amount[MAX_LEN_STR];
    char pu_datetime[MAX_LEN_STR];
    char do_datetime[MAX_LEN_STR];
    char trip_duration[MAX_LEN_STR];
    struct data_s *next;
}data_t;

typedef struct tree_s{
    data_t *data;
    struct tree_s *left;
    struct tree_s *right;
}tree_t;

tree_t *add_to_binary_search_tree(tree_t *root, data_t *data);
tree_t *add_data(tree_t *root, char *line);
tree_t *construct_tree(char datafile[], tree_t *root);
data_t *save_data(data_t *current, data_t *data);
int search_tree(tree_t *root, char *key, char *file, bool *found);
void write_search_result(char *key, data_t *data, char *file_name);
void write_to_file(char *key, data_t *data, char *file_name);
void write_not_found(char *key, char *file);
void write_all_duplicates(tree_t *root, char *file);
void read_keys(tree_t *root, char *output_file);
void malloc_fail(void);

#endif
