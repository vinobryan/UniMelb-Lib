#ifndef __AI__
#define __AI__

#include <stdint.h>
#include <unistd.h>
#include "node.h"
#include "priority_queue.h"


void initialize_ai();

move_t get_next_move(state_t init_state, int budget, 
                     propagation_t propagation, char* stats, 
                     int *expanded_nodes, 
                     int *generated_nodes, 
                     int *max_depth);
node_t *propagate_back_score_to_first_action(node_t *node);

#endif
