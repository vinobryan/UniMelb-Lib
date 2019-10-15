/**
 * Ziren Xiao
 * StudentID 675485
 */
#include "header.h"

/**
 * Find the longest process sitting in memory,
 * counted from the recent loading into memory
 * Parameters:
 *      scheduler: the scheduler after initializing
 *      time: the current time
 * Return:
 *      the details of that process
 */
detail_t *longest_process_in_memory(scheduler_t *scheduler, int time){
    int longest_time = INIT_LARGEST;
    detail_t *longest_process = NULL;
    queue_t *queue = scheduler->queue;
    while (queue!=NULL){
        if ((time - queue->start_time) >= longest_time){
            longest_time = time - queue->start_time;
            longest_process = queue->process;
            longest_process->executed_time = queue->excuted_time;
        }
        queue = queue->next;
    }
    return longest_process;
}

/**
 * Whether the first process in its quantum time
 * Parameters:
 *      queue: the queue of processes
 *      quantum: the quantum time
 * Return:
 *      true: in quantum time
 *      false: quantum time expired
 */
bool is_in_quantum_time(queue_t *queue, int quantum){
    if (queue->quantum_executed_time%quantum == NO_DIFFERENCE
        && queue->quantum_executed_time > NO_DIFFERENCE){
        return false;
    }else{
        return true;
    }
}

/**
 * Whether the first process expired
 * Parameters:
 *      queue: the queue of processes
 * Return:
 *      true: expired
 *      false: not yet expired
 */
bool is_process_expired(queue_t *queue){
    if (queue->excuted_time - queue->process->job_time >= NO_DIFFERENCE){
        return true;
    }else{
        return false;
    }
}

/**
 * The previous process in queue has expired and
 * need to turn to next one
 * Parameters:
 *      queue: the queue of processes
 * Return:
 *      a new queue with new order
 */
queue_t *run_next_queue(queue_t *queue){
    queue = add_to_queue(queue, queue->process,
                         queue->excuted_time, queue->start_time);
    queue = queue->next;
    return queue;
}

/**
 * Delete a process in a queue
 * Parameters:
 *      queue: the queue of processes
 *      process_id: the ID of the process to be deleted
 * Return:
 *      a new queue without that process
 */
queue_t *delete_from_queue(queue_t *queue, int process_id){
    if (queue==NULL){
        return queue;
    }
    if (queue->process->process_id!=process_id){
        queue->next = delete_from_queue(queue->next, process_id);
    }else{
        return queue->next;
    }
    return queue;
}

/**
 * Add a process to a queue
 * Parameters:
 *      queue: the queue of processes
 *      process: the details of the process to be added
 * Return:
 *      a new queue with that process
 */
queue_t *add_to_queue(queue_t *queue, detail_t *process,
                      int executed_time, int current_time){
    if (queue!=NULL){
        queue->next = add_to_queue(queue->next,
                                   process, executed_time, current_time);
    }else{
        if ((queue = (queue_t*)malloc(sizeof(queue_t)))==NULL){
            malloc_fail();
        }
        queue->process = process;
        queue->next = NULL;
        queue->excuted_time = executed_time;
        queue->start_time = current_time;
        queue->quantum_executed_time = INIT_EXECUTED_TIME;
        return queue;
    }
    return queue;
}
