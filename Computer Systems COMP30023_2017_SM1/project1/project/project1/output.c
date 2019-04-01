/**
 * Ziren Xiao
 * StudentID 675485
 */
#include "header.h"

void print_details_all(detail_t *root){
    // test function only
    detail_t *current = root;
    while (current!=NULL){
        printf("Create Time: %d. Process ID: %d. ",current->time_created,current->process_id);
        printf("Memory Size: %d. Job Time: %d. Executed Time: %d\n", current->memory_size, current->job_time,current->executed_time);
        current = current->next;
    }
}

void print_queue_all(queue_t *root){
    // test function only
    queue_t *current = root;
    printf("--Queue Print Starts--\n");
    while (current!=NULL){
        printf("Process ID: %d. Executed Time: %d. Quantum Time: %d. Job Time: %d. Start Time: %d\n",
               current->process->process_id,current->excuted_time,current->quantum_executed_time,current->process->job_time,current->start_time);
        current = current->next;
    }
    printf("--Queue Print Ends--\n");
}

void print_memory_all(memory_list_t *root){
    // test function only
    memory_list_t *current = root;
    printf("--Memory Print Starts--\n");
    while (current!=NULL){
        int c;
        if (current->is_occupied){
            c=1;
            printf("Occupied Process ID: %d. ", current->process_id);
        }else{
            c=0;
        }
        printf("Start: %d. End: %d. ", current->start, current->end);
        printf("Size: %d. Is Occupied: %d.\n", current->size, c);
        current = current->next;
    }
    printf("--Memory Print Ends--\n");
}

void print_swap_all(swap_list_t *root){
    // test function only
    swap_list_t *current = root;
    printf("--Swap Print Starts--\n");
    while (current!=NULL){
        printf("StartTime: %d. ProcessID: %d. \n", current->start_time, current->process->process_id);
        current = current->next;
    }
    printf("--Swap Print Ends--\n");
}

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
    int hole_count = 0;
    int process_count = 0;
    int mem_used = 0;
    memory_list_t *current = root;
    float percentage = 0;
    while (current!=NULL){
        if (current->is_occupied){
            process_count++;
            mem_used = mem_used + current->size;
        }else{
            hole_count++;
        }
        current=current->next;
    }
    percentage = (float)mem_used/mem_size*100+0.5;
    printf("time %d, %d loaded, numprocesses=%d, numholes=%d, memusage=%.f%%\n",
           time, id, process_count, hole_count, percentage);
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

