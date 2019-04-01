/**
 * Ziren Xiao
 * StudentID 675485
 */
#ifndef HEADERH
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#define MALLOC_FAIL_EXIT_CODE 1
#define MAX_LENGTH_PER_LINE 255
#define MEMORY_START_ADDRESS 0
#define INIT_EXECUTED_TIME 0
#define READ_NOTHING -1
#define NO_PROCESS -1
#define READ_NOTHING_CHAR "-1"
#define NOT_ENOUGH_SPACE -1
#define INIT_LARGEST -1
#define FIRST_FIT_ALGORITHM "first"
#define BEST_FIT_ALGORITHM "best"
#define WORST_FIT_ALGORITHM "worst"
#define FILE_NAME_ARGUMENT "-f"
#define MEMORY_SIZE_ARGUMENT "-m"
#define QUANTUM_TIME_ARGUMENT "-q"
#define ALGORITHM_NAME_ARGUMENT "-a"
#define EQUAL_STRING 0
#define NO_DIFFERENCE 0
#define NEXT_ITEM 1
#define INITIAL_POSITION 0

typedef struct detail_s{
    int time_created;
    int process_id;
    int memory_size;
    int job_time;
    int executed_time;
    struct detail_s *next;
}detail_t;

typedef struct memory_list_s{
    int start;
    int end;
    int size;
    int process_id;
    bool is_occupied;
    struct memory_list_s *next;
}memory_list_t;

typedef struct swap_list_s{
    int start_time;
    detail_t *process;
    struct swap_list_s *next;
}swap_list_t;

typedef struct queue_s{
    detail_t *process;
    int excuted_time;
    int quantum_executed_time;
    int start_time;
    struct queue_s *next;
}queue_t;

typedef struct scheduler_s{
    detail_t *process;
    memory_list_t *memory_list;
    queue_t *queue;
    swap_list_t *swap_list;
}scheduler_t;

detail_t *read_details(char *file_name);
detail_t *add_process(detail_t *root, int time_created,
                      int process_id, int memory_size, int job_time);
detail_t *longest_process_in_memory(scheduler_t *scheduler, int time);
detail_t *fetch_from_swap_list(swap_list_t *swap_root, int time_now);
memory_list_t *construct_memory_list(int memory_size);
memory_list_t *add_to_list(memory_list_t *root, detail_t *process);
memory_list_t *add_to_list_first_fit(memory_list_t *root, detail_t *process);
memory_list_t *add_to_list_worst_fit(memory_list_t *root, detail_t *process);
memory_list_t *add_to_list_best_fit(memory_list_t *root, detail_t *process);
memory_list_t *find_best_fit(memory_list_t *root, detail_t *process, int best_fit);
memory_list_t *find_worst_fit(memory_list_t *root, detail_t *process, int best_fit);
memory_list_t *delete_from_memory_list(memory_list_t *root, int process_id);
queue_t *add_to_queue(queue_t *queue, detail_t *process, int executed_time, int current_time);
queue_t *run_next_queue(queue_t *queue);
queue_t *delete_from_queue(queue_t *queue, int process_id);
void print_details_all(detail_t *root);
void print_memory_all(memory_list_t *root);
void print_queue_all(queue_t *root);
void print_swap_all(swap_list_t *root);
void malloc_fail();
void print_end(int time);
void print_info(int time, int id, memory_list_t *root, int mem_size);
char *read_algorithm_name(int argc, char **argv);
char *read_file_name(int argc, char **argv);
int read_memory_size(int argc, char **argv);
int read_quantum(int argc, char **argv);
int find_best_fit_start_address(memory_list_t *root, detail_t *process);
int find_worst_fit_start_address(memory_list_t *root, detail_t *process);
bool is_process_expired(queue_t *queue);
bool is_in_quantum_time(queue_t *queue, int quantum);
bool is_enough_memory(memory_list_t *memory_list, int size);
scheduler_t *schedule(scheduler_t *scheduler, int quantum, char *algorithm_name, int mem_size);
scheduler_t *construct_scheduler(scheduler_t *scheduler, int memory_size, char *file_name);
scheduler_t *swap_out_from_memory(scheduler_t *scheduler, int time, detail_t *process);
scheduler_t *swap_list_to_memory(scheduler_t *scheduler, int time, int mem_size, char *algorithm);
swap_list_t *delete_from_swap_list(swap_list_t *swap_root, detail_t *process);
swap_list_t *add_to_swap_list(swap_list_t *swap_root, detail_t *process, int time_now);





#endif
