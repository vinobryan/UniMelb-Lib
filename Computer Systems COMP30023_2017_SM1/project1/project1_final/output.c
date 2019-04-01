/**
 * Ziren Xiao
 * StudentID 675485
 */
#include "header.h"

/**
 * Print message each time a process loaded
 * Parameters:
 *      time: current time
 *      id: the id of process
 *      root: the root of memory list
 *      mem_size: the total size of memory
 * Return:
 *      Void
 */
void print_info(int time, int id, memory_list_t *root, int mem_size){
    int hole_count = OUTPUT_INITIAL;
    int process_count = OUTPUT_INITIAL;
    int mem_used = OUTPUT_INITIAL;
    memory_list_t *current = root;
    double percentage = OUTPUT_INITIAL;
    while (current!=NULL){
        if (current->is_occupied){
            process_count++;
            mem_used = mem_used + current->size;
        }else{
            hole_count++;
        }
        current=current->next;
    }
    
    percentage = (double) mem_used/mem_size * PERCENTAGE;
    printf("time %d, %d loaded, numprocesses=%d, numholes=%d, memusage=%.f%%\n",
           time, id, process_count, hole_count, ceil(percentage));
}

/**
 * Print message if simulation finished
 * Parameters:
 *      time: finish time
 * Return:
 *      Void
 */
void print_end(int time){
    printf("time %d, simulation finished.\n", time);
}

/**
 * Print error status and exit program when malloc fails
 * Parameters:
 *      Void
 * Return:
 *      Void
 */
void malloc_fail(){
    printf("malloc error\n");
    fflush(stdout);
    exit(MALLOC_FAIL_EXIT_CODE) ;
}

