/**
 * Ziren Xiao
 * StudentID 675485
 */
#include "header.h"

/**
 * Read file name from argument
 * Parameters:
 *      argc: total number of arguments
 *      argv: the array of arguments
 * Return:
 *      the file name
 */
char *read_file_name(int argc, char **argv){
    int index;
    for (index=INITIAL_POSITION; index<argc; index++){
        if (!strcmp(argv[index], FILE_NAME_ARGUMENT)){
            return argv[index+NEXT_ITEM];
        }
    }
    return READ_NOTHING_CHAR;
}

/**
 * Read algorithm name from argument
 * Parameters:
 *      argc: total number of arguments
 *      argv: the array of arguments
 * Return:
 *      the algorithm name
 */
char *read_algorithm_name(int argc, char **argv){
    int index;
    for (index=INITIAL_POSITION; index<argc; index++){
        if (!strcmp(argv[index], ALGORITHM_NAME_ARGUMENT)){
            return argv[index+NEXT_ITEM];
        }
    }
    return READ_NOTHING_CHAR;
}

/**
 * Read memory size from argument
 * Parameters:
 *      argc: total number of arguments
 *      argv: the array of arguments
 * Return:
 *      the memory size
 */
int read_memory_size(int argc, char **argv){
    int index;
    for (index=INITIAL_POSITION; index<argc; index++){
        if (!strcmp(argv[index], MEMORY_SIZE_ARGUMENT)){
            return atoi(argv[index+NEXT_ITEM]);
        }
    }
    return READ_NOTHING;
}

/**
 * Read quantum time from argument
 * Parameters:
 *      argc: total number of arguments
 *      argv: the array of arguments
 * Return:
 *      the quantum time
 */
int read_quantum(int argc, char **argv){
    int index;
    for (index=INITIAL_POSITION; index<argc; index++){
        if (!strcmp(argv[index], QUANTUM_TIME_ARGUMENT)){
            return atoi(argv[index+NEXT_ITEM]);
        }
    }
    return READ_NOTHING;
}

/**
 * Read process details from a file
 * Parameters:
 *      file_name: the file to be read
 * Return:
 *      a list of process details
 */
detail_t *read_details(char *file_name){
    FILE *fp = NULL;
    char line[MAX_LENGTH_PER_LINE];
    int time_created, process_id, memory_size, job_time;
    detail_t *root = NULL;
    if ((fp = fopen(file_name, "at+")) != NULL){
        while (fgets(line, MAX_LENGTH_PER_LINE, fp) != NULL){
            sscanf(line, "%d %d %d %d\n", &time_created, &process_id,
                    &memory_size, &job_time);
            root = add_process(root, time_created,
                                process_id, memory_size, job_time);
        }
    }
    fclose(fp);
    fp = NULL;
    return root;
}

/**
 * Add a process details to a list
 * Parameters:
 *      root: the root of that list
 *      time_created: the creation time of that process
 *      process_id: the id of that process
 *      memory_size: the memory size of that process
 *      job_time: the total time needed of that process
 * Return:
 *      a list of process details with the new process
 */
detail_t *add_process(detail_t *root, int time_created,
                      int process_id, int memory_size, int job_time){
    if (root!=NULL){
        root->next = add_process(root->next, time_created,
                                 process_id, memory_size, job_time);
    }else{
        if ((root = (detail_t*)malloc(sizeof(detail_t)))==NULL){
            malloc_fail();
        }
        root->time_created = time_created;
        root->process_id = process_id;
        root->memory_size = memory_size;
        root->job_time = job_time;
        root->next = NULL;
        root->executed_time = INIT_EXECUTED_TIME;
    }
    return root;
}
