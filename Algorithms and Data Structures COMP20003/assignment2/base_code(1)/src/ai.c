#include <time.h>
#include <stdlib.h>
#include <math.h>
#include "time.h"
#include "ai.h"
#include "utils.h"
#include "priority_queue.h"

struct heap h;

void initialize_ai(){
	heap_init(&h);
}

/**
 * Find best action by building all possible paths up to depth max_depth
 * and back propagate using either max or avg
 */

move_t get_next_move( uint8_t board[SIZE][SIZE], int max_depth,
                      propagation_t propagation, uint32_t record[] ){
	move_t best_action;
    node_t *first_node;
    node_t *pro_node;
    uint32_t pro_score = 0;
    /** Pro_node can be either max score node, or
     *  max average score node
     */
    pro_node = malloc(sizeof(node_t));
    first_node = malloc(sizeof(node_t));
    malloc_success(pro_node);
    malloc_success(first_node);
    board_copy(first_node->board, board);
    /** Initialize the first node */
    first_node->depth = 0;
    first_node->num_childs = NUM_DIRECTION;
    first_node->parent = NULL;
    first_node->priority = 0;
    heap_push(&h, first_node); // Add first node into PQ
    while (h.count != 0){
        node_t *node;
        node = heap_delete(&h); // Pop out a node from PQ
        record[EXPANDED_ARRAY_POSITION]++;
        if (node->depth < max_depth){
            // If not reach max depth, keep expanding
            move_to_all_direction(&h, node);
            record[GENERATED_ARRAY_POSITION]
             = record[GENERATED_ARRAY_POSITION] + NUM_DIRECTION;
        }else if (node->depth == max_depth){
            // Reach max depth, stop expanding and find corresponding node
            if (propagation == max){
                // Here processing max
                if (node->priority >= pro_score){
                    pro_score = node->priority;
                    pro_node = back_score_to_first_action(node);
                }
            }
            if (propagation == avg){
                // Here processing avg
                uint32_t temp_score = avg_score_of_all_direction(node);
                if (temp_score >= pro_score){
                    pro_score = temp_score;
                    pro_node = back_score_to_first_action(node);
                }
            }
        }
    }
    best_action = pro_node->move;
	return best_action;
}

void malloc_success(node_t *node){
    if (node == NULL){
        printf("malloc failed\n");
        exit(1);
    }
}

/**
 * Find average score of all children of this depth belongs
 * to the same parent, returns the average score
 */

uint32_t avg_score_of_all_direction(node_t *node){
    node_t *parent_node;
    parent_node = node->parent;
    uint8_t move_direction;
    uint8_t count = 0;
    uint32_t sum_score = 0;
    for (move_direction = 0; move_direction < node->num_childs; move_direction++){
        node_t *newNode;
        uint32_t score = 0;
        newNode = malloc(sizeof(node_t));
        malloc_success(newNode);
        board_copy(newNode->board, parent_node->board);
        execute_move_t( newNode->board, &score, move_direction );
        if ( newNode->board != node->board){
            sum_score = sum_score + score;
            count++;
        }
        free(newNode);
    }
    return sum_score / count;
}

/**
 * Move to all directions, and store valid direction
 * into a priority queue
 */

void move_to_all_direction(struct heap *h, node_t *node){
    uint8_t move_direction;
    for (move_direction = 0; move_direction < node->num_childs; move_direction++){
        uint32_t score = node->priority;
        bool success;
        node_t *newNode;
        /** Add a if = null */
        newNode = malloc(sizeof(node_t));
        malloc_success(newNode);
        board_copy(newNode->board, node->board);
        success = execute_move_t( newNode->board, &score, move_direction ); // Apply an action
        if (success){
            addRandom(newNode->board);
            newNode->priority = score;
            newNode->parent = node;
            newNode->move = move_direction;
            newNode->depth = node->depth + 1;
            newNode->num_childs = node->num_childs;
            if ( newNode->board != node->board){
                heap_push(h, newNode);
            }else{
                free(newNode);
            }
        }
    }
}

/**
 * Find the second child in this graph(or tree),
 * return the node of that child
 */

node_t *back_score_to_first_action(node_t *node){
    if (node->parent->parent != NULL){ // Not reached the second child
        return back_score_to_first_action(node->parent);
    }else{ // Reached the second child
        return node;
    }
}

/**
 * Copy the content of board b into board a
 *
 */

void board_copy(uint8_t a[SIZE][SIZE], uint8_t b[SIZE][SIZE]){
    int i, j;
    for (i=0; i<SIZE; i++){
        for (j=0; j<SIZE; j++){
            a[i][j] = b[i][j];
        }
    }
}

/**
 * Find the max tile in a board, return the power of the max
 * tile (based on 2, eg. 2^x, here x is returning value)
 */

uint8_t find_max_tile(uint8_t board[SIZE][SIZE]){
    uint8_t max_tile = 0;
    int i, j;
    for (i=0; i<SIZE; i++){
        for (j=0; j<SIZE; j++){
            if (board[i][j] > max_tile){
                max_tile = board[i][j];
            }
        }
    }
    return max_tile;
}

/**
 * Write running results into specific file, including
 * time duration, max tile(2^x, not only the power), score
 * max depth, number of generated and expanded nodes.
 */

void write_data(double duration, uint8_t max_tile,
                uint32_t score, int max_depth, uint32_t record[]){
    uint32_t expanded_per_second;
    double max_tile_score;
    char temp[MAX_DATA_LENGTH];
    FILE *fp = fopen(FILE_NAME, "a+");
    if(fp==NULL){
        printf("Cannot open file!");
        exit(1);
    }
    expanded_per_second = record[EXPANDED_ARRAY_POSITION] / duration;
    fputs("MaxDepth = ", fp);
    sprintf(temp, "%d", max_depth);
    fputs(temp, fp);
    fputs("\n", fp);

    fputs("Generated = ", fp);
    sprintf(temp, "%d", record[GENERATED_ARRAY_POSITION]);
    fputs(temp, fp);
    fputs("\n", fp);

    fputs("Expanded = ", fp);
    sprintf(temp, "%d", record[EXPANDED_ARRAY_POSITION]);
    fputs(temp, fp);
    fputs("\n", fp);

    fputs("Time = ", fp);
    sprintf(temp, "%.02f", duration);
    fputs(temp, fp);
    fputs(" seconds", fp);
    fputs("\n", fp);

    fputs("Expanded/Second = ", fp);
    sprintf(temp, "%d", expanded_per_second);
    fputs(temp, fp);
    fputs("\n", fp);

    fputs("max_tile = ", fp);
    max_tile_score = pow((double) POWER_BASE, (double) max_tile);
    sprintf(temp, "%d", (int)max_tile_score);
    fputs(temp, fp);
    fputs("\n", fp);

    fputs("Score = ", fp);
    sprintf(temp, "%d", score);
    fputs(temp, fp);
    fputs("\n", fp);

    fclose(fp);
}

