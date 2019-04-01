/**
 * Ziren Xiao
 * StudentID 675485
 */
#include "header.h"

/**
 * Initialize a scheduler
 * Parameters:
 *      scheduler: the scheduler needed to be initialize
 *      memory_size: the size of total memory
 *      file_name: the input file name
 * Return:
 *      constructed scheduler
 */
scheduler_t *construct_scheduler(scheduler_t *scheduler,
                                 int memory_size, char *file_name){
    if ((scheduler = (scheduler_t*)malloc(sizeof(scheduler_t)))==NULL){
        malloc_fail();
    }
    scheduler->queue = NULL;
    scheduler->swap_list = NULL;
    scheduler->memory_list = construct_memory_list(memory_size);
    scheduler->process = read_details(file_name);
    return scheduler;
}

/**
 * Simulate the cycle of Round Robin
 * Parameters:
 *      scheduler: the scheduler after initializing
 *      quantum: the RRQ quantum time
 *      algorithm_name: the one name of three algorithms
 *      mem_size: the size of total memory
 * Return:
 *      executed scheduler
 */
scheduler_t *schedule(scheduler_t *scheduler,
                      int quantum, char *algorithm_name, int mem_size){
    int time = INIT_EXECUTED_TIME;
    bool is_end = false, is_first = true;
    detail_t *current_read_process = scheduler->process;
    while (!is_end){
        //printf("-----------Time %d----------\n", time);
        /** New Process */
        if (current_read_process!=NULL){
            while (current_read_process->time_created == time
                   &&current_read_process!=NULL){
                scheduler->swap_list = add_to_swap_list(scheduler->swap_list,
                                                        current_read_process, time);
                current_read_process = current_read_process->next;
                if (current_read_process==NULL){
                    break;
                }
            }
        }
        if (is_first){
            scheduler = swap_list_to_memory(scheduler, time, mem_size, algorithm_name);
            is_first = false;
        }
        /** Current Process */
        if (is_process_expired(scheduler->queue)){
        /** Expired Process
        *   if process reaches job time, remove from all list
        */
            scheduler->memory_list = delete_from_memory_list(scheduler->memory_list,
                                                             scheduler->queue->process->process_id);
            scheduler->queue = delete_from_queue(scheduler->queue,
                                                 scheduler->queue->process->process_id);
            scheduler = swap_list_to_memory(scheduler, time, mem_size,algorithm_name);
            if (scheduler->queue==NULL&&scheduler->swap_list==NULL){
                is_end = true;
                print_end(time);
            }
            //printf("--Delete executed--\n");
        }
        if (!is_end){
            /** if not finish running */
            if(!is_in_quantum_time(scheduler->queue, quantum)){
            /** if quantum expired, turn to the next */
                scheduler = swap_list_to_memory(scheduler, time, mem_size,algorithm_name);
                scheduler->queue->quantum_executed_time = INIT_EXECUTED_TIME;
                scheduler->queue = run_next_queue(scheduler->queue);
            }
            scheduler->queue->excuted_time++;
            scheduler->queue->quantum_executed_time++;
        }


        //print_memory_all(scheduler->memory_list);
        //print_queue_all(scheduler->queue);
        //print_swap_all(scheduler->swap_list);
        time++;
    }
    return scheduler;
}

/**
 * Load process from swap list to memory list
 * Parameters:
 *      scheduler: the scheduler after initializing
 *      time: current running time
 *      algorithm: the one name of three algorithms
 *      mem_size: the size of total memory
 * Return:
 *      added process scheduler
 */
scheduler_t *swap_list_to_memory(scheduler_t *scheduler,
                                 int time, int mem_size, char *algorithm){
    if (scheduler->swap_list!=NULL){
        detail_t *read_process =
                        fetch_from_swap_list(scheduler->swap_list, time);
        scheduler->swap_list =
                        delete_from_swap_list(scheduler->swap_list, read_process);
        if (!is_enough_memory(scheduler->memory_list, read_process->memory_size)){
            /** Not enough memory */
            scheduler = swap_out_from_memory(scheduler, time, read_process);
        }
        /** Select proper algorithm */
        if (strcmp(algorithm, BEST_FIT_ALGORITHM)==EQUAL_STRING){
            scheduler->memory_list =
                            add_to_list_best_fit(scheduler->memory_list, read_process);
        }else if (strcmp(algorithm, WORST_FIT_ALGORITHM)==EQUAL_STRING){
            scheduler->memory_list =
                            add_to_list_worst_fit(scheduler->memory_list, read_process);
        }else if (strcmp(algorithm, FIRST_FIT_ALGORITHM)==EQUAL_STRING){
            scheduler->memory_list =
                            add_to_list_first_fit(scheduler->memory_list, read_process);
        }
        scheduler->queue = add_to_queue(scheduler->queue,
                                        read_process, read_process->executed_time, time);
        print_info(time, read_process->process_id,
                   scheduler->memory_list, mem_size);
    }
    return scheduler;
}


/**
 * When memory is not enough, swap out some process from memory
 * Parameters:
 *      scheduler: the scheduler after initializing
 *      time: current running time
 *      process: the process that to be insert into memory
 * Return:
 *      enough space scheduler
 */
scheduler_t *swap_out_from_memory(scheduler_t *scheduler, int time, detail_t *process){
    while (!is_enough_memory(scheduler->memory_list, process->memory_size)){
        detail_t *swap_out_process = longest_process_in_memory(scheduler, time);
        scheduler->memory_list = delete_from_memory_list(scheduler->memory_list,
                                                         swap_out_process->process_id);
        scheduler->queue = delete_from_queue(scheduler->queue, swap_out_process->process_id);
        scheduler->swap_list = add_to_swap_list(scheduler->swap_list, swap_out_process, time);
    }
    /** Enough memory to add new item */
    return scheduler;
}
