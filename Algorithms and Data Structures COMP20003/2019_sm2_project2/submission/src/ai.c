#include <time.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>


#include "ai.h"
#include "utils.h"
#include "priority_queue.h"


struct heap h;

float get_reward( node_t* n );

/**
 * Function called by pacman.c
*/
void initialize_ai(){
	heap_init(&h);
}

/**
 * function to copy a src into a dst state
*/
void copy_state(state_t* dst, state_t* src){
	//Location of Ghosts and Pacman
	memcpy( dst->Loc, src->Loc, 5*2*sizeof(int) );

    //Direction of Ghosts and Pacman
	memcpy( dst->Dir, src->Dir, 5*2*sizeof(int) );

    //Default location in case Pacman/Ghosts die
	memcpy( dst->StartingPoints, src->StartingPoints, 5*2*sizeof(int) );

    //Check for invincibility
    dst->Invincible = src->Invincible;
    
    //Number of pellets left in level
    dst->Food = src->Food;
    
    //Main level array
	memcpy( dst->Level, src->Level, 29*28*sizeof(int) );

    //What level number are we on?
    dst->LevelNumber = src->LevelNumber;
    
    //Keep track of how many points to give for eating ghosts
    dst->GhostsInARow = src->GhostsInARow;

    //How long left for invincibility
    dst->tleft = src->tleft;

    //Initial points
    dst->Points = src->Points;

    //Remiaining Lives
    dst->Lives = src->Lives;   

}

node_t* create_init_node( state_t* init_state ){
	node_t * new_n = (node_t *) malloc(sizeof(node_t));
	new_n->parent = NULL;
	new_n->priority = 0;
	new_n->depth = 0;
	new_n->num_childs = 4;
	copy_state(&(new_n->state), init_state);
	new_n->acc_reward = get_reward(new_n);
	return new_n;
	
}

bool lost_life(node_t *n){
    if (n->parent == NULL){
        return false;
    }
    state_t old = n->state;
    state_t new = n->parent->state;
    return (new.Lives - old.Lives) != 0;
}

float heuristic( node_t* n ){
	float h = 0;
	
	//FILL IN MISSING CODE
    float i = 0, l = 0, g = 0;

    if (n->parent != NULL){
        if (n->state.Invincible && !n->parent->state.Invincible){
            i = 10;
        }
    }
    
    if (lost_life(n)){
        l = 10;
    }
    
    if (n->state.Lives == 0){
        g = 100;
    }

    h = i + l + g;
	return h;
}

float get_reward ( node_t* n ){
	float reward = 0;
	
	//FILL IN MISSING CODE

	float discount = pow(0.99,n->depth);
    
    float parent_point = 0;
    
    if (n->parent != NULL){
        parent_point = n->parent->state.Points;
    }
    
    reward = (heuristic(n) + n->state.Points - parent_point) * discount;

	return reward;
}

/**
 * Apply an action to node n and return a new node resulting from executing the action
*/
bool applyAction(node_t* n, node_t **new_node, move_t action){

    //FILL IN MISSING CODE
    (*new_node) = malloc(sizeof(node_t));
    
    (*new_node)->parent = n;
    
    (*new_node)->move = action;
    
    (*new_node)->priority = -(n->depth);
    
    (*new_node)->depth = n->depth + 1;
    
    (*new_node)->num_childs = 4;
    
    copy_state(&((*new_node)->state), &(n->state));

    (*new_node)->acc_reward = get_reward(*new_node);

    return execute_move_t( &((*new_node)->state), action );

}

move_t random_select_largest(float best_action_score[]){
    float largest_score = INT_MIN;
    for(unsigned i = 0; i < 4; i++){
        if (best_action_score[i] > largest_score){
            largest_score = best_action_score[i];
        }
    }
    
    unsigned free_position = 0;
    unsigned best_actions[4];
    for(unsigned i = 0; i < 4; i++){
        if (best_action_score[i] >= largest_score){
            best_actions[free_position] = i;
            free_position++;
        }
    }
    
    return best_actions[rand()%free_position];
}


/**
 * Find best action by building all possible paths up to budget
 * and back propagate using either max or avg
 */

move_t get_next_move( state_t init_state, int budget, propagation_t propagation, char* stats ){
    float best_action_score[4];
    for(unsigned i = 0; i < 4; i++)
        best_action_score[i] = INT_MIN;

    unsigned generated_nodes = 0;
    unsigned expanded_nodes = 0;
    unsigned max_depth = 0;
    move_t best_action;
    
    node_t* first_node = create_init_node(&init_state);
    heap_push(&h, first_node);
    
    while (h.count != 0){
        node_t *pop_node = heap_delete(&h);
        expanded_nodes ++;
        if (expanded_nodes < budget){
            for (unsigned move = 0; move < pop_node->num_childs; move++){
                bool success;
                node_t *new_node;
                generated_nodes ++;
                success = applyAction(pop_node, &new_node, move);
                
                node_t *propagate_node = propagate_back_score_to_first_action(new_node);
                move_t propagate_move = propagate_node->move;
                if (new_node->state.Points > best_action_score[propagate_move]){
                    best_action_score[propagate_move] = new_node->state.Points;
                }
                
                if (success){
                    if (!lost_life(new_node)){
                        heap_push(&h, new_node);
                    }else{
                        free(new_node);
                    }
                }else{
                    free(new_node);
                }
            }
        }
    }

    best_action = random_select_largest(best_action_score);
    
    
	sprintf(stats, "Max Depth: %d Expanded nodes: %d  Generated nodes: %d\n",max_depth,expanded_nodes,generated_nodes);
	
	if(best_action == left)
		sprintf(stats, "%sSelected action: Left\n",stats);
	if(best_action == right)
		sprintf(stats, "%sSelected action: Right\n",stats);
	if(best_action == up)
		sprintf(stats, "%sSelected action: Up\n",stats);
	if(best_action == down)
		sprintf(stats, "%sSelected action: Down\n",stats);

	sprintf(stats, "%sScore Left %f Right %f Up %f Down %f",stats,best_action_score[left],best_action_score[right],best_action_score[up],best_action_score[down]);
	return best_action;
}

node_t *propagate_back_score_to_first_action(node_t *node){
    if (node->parent->parent != NULL){ 
        // Not reached the second child
        return propagate_back_score_to_first_action(node->parent);
    }else{ 
        // Reached the second child
        return node;
    }
}