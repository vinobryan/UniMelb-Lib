#ifndef __AI__
#define __AI__

#include <stdint.h>
#include <unistd.h>
#include "node.h"
#include "priority_queue.h"

#define POWER_BASE 2
#define NUM_DIRECTION 4
#define SEEK_END 2
#define WRITE_FILE_MOVE 0
#define FILE_OPEN_FAIL 0
#define NUM_DATA_STORE 2
#define GENERATED_ARRAY_POSITION 0
#define EXPANDED_ARRAY_POSITION 1
#define FILE_NAME "output.txt"
#define MAX_DATA_LENGTH 32

void initialize_ai();

move_t get_next_move( uint8_t board[SIZE][SIZE],
                     int max_depth, propagation_t propagation, uint32_t record[] );
node_t *back_score_to_first_action(node_t *node);
void move_to_all_direction(struct heap *h, node_t *node);
void write_data(double duration, uint8_t max_tile,
                uint32_t score, int max_depth, uint32_t record[]);
void board_copy(uint8_t a[SIZE][SIZE], uint8_t b[SIZE][SIZE]);
void malloc_success(node_t *node);
uint8_t find_max_tile(uint8_t board[SIZE][SIZE]);
uint32_t avg_score_of_all_direction(node_t *node);

#endif
