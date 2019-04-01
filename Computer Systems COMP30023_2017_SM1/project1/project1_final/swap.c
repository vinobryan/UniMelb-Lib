/**
 * Ziren Xiao
 * StudentID 675485
 */
#include "header.h"

/**
 * Fetch a process from swap list, which is longest
 * on the disk
 * Parameters:
 *      swap_root: the root of swap list
 *      time_now: the current time
 * Return:
 *      the details of that process
 */
detail_t *fetch_from_swap_list(swap_list_t *swap_root, int time_now){
    /** Fetch the one sitting longest */
    swap_list_t *root = swap_root;
    int longest_time = INIT_LARGEST;
    detail_t *process = NULL;
    while (root!=NULL){
        if (time_now - root->start_time > longest_time){
            process = root->process;
            longest_time = time_now - root->start_time;
        }else if(time_now - root->start_time == longest_time){
            if (process->process_id>root->process->process_id){
                process = root->process;
                longest_time = time_now - root->start_time;
            }
        }
        root = root->next;
    }
    return process;
}

/**
 * Delete a process from swap list
 * Parameters:
 *      swap_root: the root of swap list
 *      process: the process to be deleted
 * Return:
 *      the new swap list without that process
 */
swap_list_t *delete_from_swap_list(swap_list_t *swap_root, detail_t *process){
    if (swap_root==NULL){
        return swap_root;
    }
    if (swap_root->process->process_id!=process->process_id){
        swap_root->next = delete_from_swap_list(swap_root->next, process);
    }else{
        return swap_root->next;
    }
    return swap_root;
}

/**
 * Add a process to swap list
 * Parameters:
 *      swap_root: the root of swap list
 *      process: the process to be added
 *      time_now: the current time
 * Return:
 *      the new swap list with that process
 */
swap_list_t *add_to_swap_list(swap_list_t *swap_root, detail_t *process, int time_now){
    if (swap_root!=NULL){
        swap_root->next = add_to_swap_list(swap_root->next, process, time_now);
    }else{
        if ((swap_root = (swap_list_t*)malloc(sizeof(swap_list_t)))==NULL){
            malloc_fail();
        }
        swap_root->process = process;
        swap_root->start_time = time_now;
        swap_root->next = NULL;
        return swap_root;
    }
    return swap_root;
}
