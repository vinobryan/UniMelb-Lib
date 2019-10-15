/**
 * Ziren Xiao
 * StudentID 675485
 */
#include "header.h"

/**
 * Delete a process from memory list, and merge
 * adjacent hole(s).
 * Parameters:
 *      root: the root of memory list
 *      process_id: the ID of the process to be deleted
 * Return:
 *      the new memory list without that process
 */
memory_list_t *delete_from_memory_list(memory_list_t *root, int process_id){
    memory_list_t *current = root, *last = NULL;
    while (current!=NULL){
        if (current->process_id != process_id){
            last = current;
            current = current->next;
        }else{
            if (last!=NULL && !last->is_occupied &&
                current->next!=NULL && !current->next->is_occupied){
                /** if both the last and next one are not occupied */
                last->next = current->next->next;
                last->start = current->next->start;
                last->size = last->end - current->next->start;
            }else{
                /** if not both */
                if (last!=NULL && !last->is_occupied){
                /** if the last is not occupied */
                    last->next = current->next;
                    last->start = current->start;
                    last->size = last->end - current->start;
                }
                if (current->next!=NULL && !current->next->is_occupied){
                /** if the next is not occupied */   
                    current->next->end = current->end;
                    current->next->size = current->end - current->next->start;
                    if (last!=NULL){
                        last->next = current->next;
                    }else{
                        root = current->next;
                    }
                    current = current->next;
                }
            }
            current->is_occupied = false;
            current->process_id = NO_PROCESS;
            break;
        }
    }
    return root;
}

/**
 * Find the start address using best fit algorithm
 * of a process
 * Parameters:
 *      root: the root of memory list
 *      process: the process to be inserted
 * Return:
 *      the right position in memory list
 */
int find_best_fit_start_address(memory_list_t *root, detail_t *process){
    memory_list_t *current = root;
    int min_difference = INT_MAX;
    int start_address;
    while (current!=NULL){
        if (!current->is_occupied && current->size >= process->memory_size
            && current->size - process->memory_size < min_difference){
            min_difference = current->size - process->memory_size;
            start_address = current->start;
        }
        if (min_difference == NO_DIFFERENCE){
            break;
        }
        current = current->next;
    }
    if (min_difference == INT_MAX){
        return NOT_ENOUGH_SPACE;
    }else{
        return start_address;
    }
}

/**
 * Find the start address using worst fit algorithm
 * of a process
 * Parameters:
 *      root: the root of memory list
 *      process: the process to be inserted
 * Return:
 *      the right position in memory list
 */
int find_worst_fit_start_address(memory_list_t *root, detail_t *process){
    memory_list_t *current = root;
    int max_memory = NOT_ENOUGH_SPACE;
    int start_address;
    while (current!=NULL){
        if (!current->is_occupied && current->size >= process->memory_size
            && current->size > max_memory){
            max_memory = current->size ;
            start_address = current->start;
        }
        current = current->next;
    }
    if (max_memory == NOT_ENOUGH_SPACE){
        return NOT_ENOUGH_SPACE;
    }else{
       // printf("Start address: %d\n", start_address);
        return start_address;
    }
}

/**
 * Add a process into the memory list. Here may split
 * a block into 2 blocks if process not completely fit
 * that position.
 * Parameters:
 *      root: the insert position of memory list
 *      process: the process to be inserted
 * Return:
 *      a part memory list with this process
 */
memory_list_t *add_to_list(memory_list_t *root, detail_t *process){
    memory_list_t *split_a, *split_b;
    if (process->memory_size == root->size){
        root->process_id = process->process_id;
        root->is_occupied = true;
        return root;
    }
    if ((split_a = (memory_list_t*)malloc(sizeof(memory_list_t)))==NULL){
        malloc_fail();
    }
    if ((split_b = (memory_list_t*)malloc(sizeof(memory_list_t)))==NULL){
        malloc_fail();
    }
    split_a->start = root->end - process->memory_size;
    split_a->end = root->end;
    split_a->size = process->memory_size;
    split_a->is_occupied = true;
    split_a->process_id = process->process_id;
    split_a->next = split_b;
    split_b->start = root->start;
    split_b->end = split_a->start;
    split_b->size = root->size - process->memory_size;
    split_b->is_occupied = false;
    split_b->process_id = NO_PROCESS;
    split_b->next = root->next;
    return split_a;
}

/**
 * Add a process in to memory using best fit algorithm
 * Parameters:
 *      root: the root of memory list
 *      process: the process to be inserted
 *      best_fit: the start address of memory with best fit
 * Return:
 *      a memory list with that process
 */
memory_list_t *find_best_fit(memory_list_t *root, detail_t *process, int best_fit){
    if (root->start != best_fit){
        root->next = find_best_fit(root->next, process, best_fit);
    }else{
        return add_to_list(root, process);
    }
    return root;
}

/**
 * Add a process in to memory using worst fit algorithm
 * Parameters:
 *      root: the root of memory list
 *      process: the process to be inserted
 *      best_fit: the start address of memory with worst fit
 * Return:
 *      a memory list with that process
 */
memory_list_t *find_worst_fit(memory_list_t *root, detail_t *process, int worst_fit){
    if (root->start != worst_fit){
        root->next = find_worst_fit(root->next, process, worst_fit);
    }else{
        return add_to_list(root, process);
    }
    return root;
}

/**
 * Find best fit position and then use this position add a
 * process into memory list
 * Parameters:
 *      root: the root of memory list
 *      process: the process to be inserted
 * Return:
 *      a memory list with that process
 */
memory_list_t *add_to_list_best_fit(memory_list_t *root, detail_t *process){
    int best_fit = find_best_fit_start_address(root, process);
    return find_best_fit(root, process, best_fit);
}

/**
 * Find worst fit position and then use this position add a
 * process into memory list
 * Parameters:
 *      root: the root of memory list
 *      process: the process to be inserted
 * Return:
 *      a memory list with that process
 */
memory_list_t *add_to_list_worst_fit(memory_list_t *root, detail_t *process){
    int worst_fit = find_worst_fit_start_address(root, process);
    return find_worst_fit(root, process, worst_fit);
}

/**
 * Add a process into memory list using first fit
 * algorithm
 * Parameters:
 *      root: the root of memory list
 *      process: the process to be inserted
 * Return:
 *      a memory list with that process
 */
memory_list_t *add_to_list_first_fit(memory_list_t *root, detail_t *process){
    if (root->size < process->memory_size || root->is_occupied){
        root->next = add_to_list_first_fit(root->next, process);
    }else{
        return add_to_list(root, process);
    }
    return root;
}

/**
 * Initialize a memory list
 * Parameters:
 *      memory_size: the total size of the memory
 * Return:
 *      a constructed memory list
 */
memory_list_t *construct_memory_list(int memory_size){
    memory_list_t *root;
    if ((root = (memory_list_t*)malloc(sizeof(memory_list_t)))==NULL){
        malloc_fail();
    }
    root->start = MEMORY_START_ADDRESS;
    root->end = memory_size;
    root->size = memory_size;
    root->is_occupied = false;
    root->next = NULL;
    root->process_id = NO_PROCESS;
    return root;
}

/**
 * Whether the memory is enough for a size
 * Parameters:
 *      root: the root of memory list
 *      size: the size that to be compared
 * Return:
 *      true: with enough memory
 *      false: with not enough memory
 */
bool is_enough_memory(memory_list_t *memory_list, int size){
    if (memory_list==NULL){
        return false;
    }
    if (memory_list->is_occupied || memory_list->size<size){
        return is_enough_memory(memory_list->next, size);
    }
    return true;
}
