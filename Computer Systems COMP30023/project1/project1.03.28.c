#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>

#define MALLOC_FAIL_EXIT_CODE 1
#define MAX_LENGTH_PER_LINE 255
#define MEMORY_START_ADDRESS 0
#define INIT_EXECUTED_TIME 0
#define READ_NOTHING -1
#define NO_PROCESS -1
#define READ_NOTHING_CHAR "-1"
#define NOT_ENOUGH_SPACE -1

typedef struct detail_s{
    int time_created;
    int process_id;
    int memory_size;
    int job_time;
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
    int process_id;
    struct swap_list_s *next;
}swap_list_t;

typedef struct queue_s{
    detail_t *process;
    int excuted_time;
    int quantum_executed_time;
    struct queue_s *next;
}queue_t;

typedef struct scheduler_s{
    detail_t *process;
    memory_list_t *memory_list;
    queue_t *queue;
}scheduler_t;

detail_t *read_details(char *file_name);
detail_t *add_process(detail_t *root, int time_created,
                      int process_id, int memory_size, int job_time);
detail_t *get_process(detail_t *root, int process_id);
memory_list_t *construct_memory_list(int memory_size);
memory_list_t *add_to_list(memory_list_t *root, detail_t *process);
memory_list_t *add_to_list_first_fit(memory_list_t *root, detail_t *process);
memory_list_t *add_to_list_worst_fit(memory_list_t *root, detail_t *process);
memory_list_t *add_to_list_best_fit(memory_list_t *root, detail_t *process);
memory_list_t *find_best_fit(memory_list_t *root, detail_t *process, int best_fit);
memory_list_t *find_worst_fit(memory_list_t *root, detail_t *process, int best_fit);
memory_list_t *delete_from_memory_list(memory_list_t *root, int process_id);
queue_t *add_to_queue(queue_t *queue, detail_t *process, int executed_time);
queue_t *run_next_queue(queue_t *queue);
void print_details_all(detail_t *root);
void print_memory_all(memory_list_t *root);
void print_queue_all(queue_t *root);
void malloc_fail();
char *read_algorithm_name(int argc, char **argv);
char *read_file_name(int argc, char **argv);
int read_memory_size(int argc, char **argv);
int read_quantum(int argc, char **argv);
int find_best_fit_start_address(memory_list_t *root, detail_t *process);
int find_worst_fit_start_address(memory_list_t *root, detail_t *process);
bool is_process_expired(queue_t *queue);
bool is_in_quantum_time(queue_t *queue, int quantum);
scheduler_t *schedule(scheduler_t *scheduler, int quantum, char *algorithm_name);
scheduler_t *construct_scheduler(scheduler_t *scheduler, int memory_size, char *file_name);

int main(int argc, char **argv) {
    int quantum, memory_size;
    char *algorithm_name, *file_name;
    scheduler_t *scheduler;

    quantum = read_quantum(argc, argv);
    printf("Quantum = %d.\n", quantum);
    memory_size = read_memory_size(argc, argv);
    printf("MemorySize = %d.\n", memory_size);
    algorithm_name = read_algorithm_name(argc, argv);
    printf("Method = %s.\n", algorithm_name);
    file_name = read_file_name(argc, argv);
    printf("FileName = %s.\n", file_name);

    scheduler = construct_scheduler(scheduler, memory_size, file_name);
    print_details_all(scheduler->process);
    scheduler = schedule(scheduler, quantum, algorithm_name);
}

scheduler_t *construct_scheduler(scheduler_t *scheduler, int memory_size, char *file_name){
    if ((scheduler = (scheduler_t*)malloc(sizeof(scheduler_t)))==NULL){
        malloc_fail();
    }
    scheduler->queue = NULL;
    scheduler->memory_list = construct_memory_list(memory_size);
    scheduler->process = read_details(file_name);
    return scheduler;
}

scheduler_t *schedule(scheduler_t *scheduler, int quantum, char *algorithm_name){
    int time = INIT_EXECUTED_TIME;
    bool is_end = false;
    detail_t *current_read_process = scheduler->process;
    memory_list_t *mem_list = scheduler->memory_list;
    queue_t *queue = scheduler->queue;
    while (!is_end){
        printf("-----------Time %d----------\n", time);
        /** New Process */
        if (scheduler->process!=NULL){
            if (scheduler->process->time_created==time){
                scheduler->memory_list = add_to_list_best_fit(scheduler->memory_list, scheduler->process);
                queue = add_to_queue(queue, scheduler->process, INIT_EXECUTED_TIME);
                scheduler->process = scheduler->process->next;
            }
        }
        /** Current Process
        *   if the first process in Round Robin Queue has not
        *   expired yet
        */
        if (is_process_expired(queue) && !is_end){
        /** Expired Process
        *   if process reaches job time, remove from all list
        */
            scheduler->memory_list = delete_from_memory_list(scheduler->memory_list, queue->process->process_id);
            queue = queue->next;
            if (queue==NULL){
                is_end = true;
            }
            printf("--Delete executed--\n");

        }
        if (!is_end){
            /** if not finish running */
            if(!is_in_quantum_time(queue, quantum)){
            /** if quantum expired, turn to the next */
                queue->quantum_executed_time = INIT_EXECUTED_TIME;
                queue = run_next_queue(queue);
            }else{
                queue->excuted_time++;
                queue->quantum_executed_time++;
            }
        }
        print_memory_all(scheduler->memory_list);
        print_queue_all(queue);
        time++;
    }
    return scheduler;
}

bool is_in_quantum_time(queue_t *queue, int quantum){
    if (queue->quantum_executed_time%quantum == 0 && queue->quantum_executed_time>0){
        return false;
    }else{
        return true;
    }
}

bool is_process_expired(queue_t *queue){
    if (queue->excuted_time - queue->process->job_time >= 0){
        return true;
    }else{
        return false;
    }
}

queue_t *run_next_queue(queue_t *queue){
    queue = add_to_queue(queue, queue->process, queue->excuted_time);
    queue = queue->next;
    return queue;
}


queue_t *add_to_queue(queue_t *queue, detail_t *process, int executed_time){
    if (queue!=NULL){
        queue->next = add_to_queue(queue->next, process, executed_time);
    }else{
        if ((queue = (queue_t*)malloc(sizeof(queue_t)))==NULL){
            malloc_fail();
        }
        queue->process = process;
        queue->next = NULL;
        queue->excuted_time = executed_time;
        queue->quantum_executed_time = INIT_EXECUTED_TIME;
        return queue;
    }
    return queue;
}

memory_list_t *delete_from_memory_list(memory_list_t *root, int process_id){
    memory_list_t *current = root, *last = NULL;
    while (current!=NULL){
        if (current->process_id != process_id){
            last = current;
            current = current->next;
        }else{
            if (last!=NULL && !last->is_occupied &&
                current->next!=NULL && !current->next->is_occupied){
                last->next = current->next->next;
                last->start = current->next->start;
                last->size = last->end - current->next->start;
            }else{
                if (last!=NULL && !last->is_occupied){
                    last->next = current->next;
                    last->start = current->start;
                    last->size = last->end - current->start;
                }
                if (current->next!=NULL && !current->next->is_occupied){
                    last->next = current->next;
                    current->next->end = current->end;
                    current->next->size = current->end - current->next->start;
                }
            }
            current->is_occupied = false;
            break;
        }
    }
    return root;
}

int find_best_fit_start_address(memory_list_t *root, detail_t *process){
    memory_list_t *current = root;
    int min_difference = INT_MAX;
    int start_address;
    while (current!=NULL){
        if (!current->is_occupied && current->size > process->memory_size
            && current->size - process->memory_size < min_difference){
            min_difference = current->size - process->memory_size;
            start_address = current->start;
        }
        if (min_difference == 0){
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

int find_worst_fit_start_address(memory_list_t *root, detail_t *process){
    memory_list_t *current = root;
    int max_memory = NOT_ENOUGH_SPACE;
    int start_address;
    while (current!=NULL){
        if (!current->is_occupied && current->size > process->memory_size
            && current->size > max_memory){
            max_memory = current->size;
            start_address = current->start;
        }
        current = current->next;
    }
    if (max_memory == NOT_ENOUGH_SPACE){
        return NOT_ENOUGH_SPACE;
    }else{
        return start_address;
    }
}

memory_list_t *add_to_list(memory_list_t *root, detail_t *process){
    memory_list_t *split_a, *split_b;
    if ((split_a = (memory_list_t*)malloc(sizeof(memory_list_t)))==NULL){
        malloc_fail();
    }
    if ((split_b = (memory_list_t*)malloc(sizeof(memory_list_t)))==NULL){
        malloc_fail();
    }
    split_a->start = root->start;
    split_a->end = process->memory_size + root->start;
    split_a->size = split_a->end - split_a->start;
    split_a->is_occupied = true;
    split_a->next = root->next;
    split_a->process_id = process->process_id;
    split_b->start = split_a->end;
    split_b->end = root->end;
    split_b->size = split_b->end - split_b->start;
    split_b->is_occupied = false;
    split_b->next = split_a;
    split_b->process_id = NO_PROCESS;
    return split_b;
}

memory_list_t *add_to_list_best_fit(memory_list_t *root, detail_t *process){
    int best_fit = find_best_fit_start_address(root, process);
    // if best_fit = -1;
    return find_best_fit(root, process, best_fit);
}

memory_list_t *find_best_fit(memory_list_t *root, detail_t *process, int best_fit){
    if (root->start != best_fit){
        root->next = find_best_fit(root->next, process, best_fit);
    }else{
        return add_to_list(root, process);
    }
}

memory_list_t *find_worst_fit(memory_list_t *root, detail_t *process, int worst_fit){
    if (root->start != worst_fit){
        root->next = find_worst_fit(root->next, process, worst_fit);
    }else{
        return add_to_list(root, process);
    }
}

memory_list_t *add_to_list_worst_fit(memory_list_t *root, detail_t *process){
    int worst_fit = find_worst_fit_start_address(root, process);
    return find_worst_fit(root, process, worst_fit);
}

memory_list_t *add_to_list_first_fit(memory_list_t *root, detail_t *process){
    if (root->size < process->memory_size || root->is_occupied){
        root->next = add_to_list_first_fit(root->next, process);
    }else{
        return add_to_list(root, process);
    }
}

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

char *read_file_name(int argc, char **argv){
    int index;
    for (index=0; index<argc; index++){
        if (!strcmp(argv[index],"-f")){
            return argv[index+1];
        }
    }
    return READ_NOTHING_CHAR;
}

char *read_algorithm_name(int argc, char **argv){
    int index;
    for (index=0; index<argc; index++){
        if (!strcmp(argv[index],"-a")){
            return argv[index+1];
        }
    }
    return READ_NOTHING_CHAR;
}

int read_memory_size(int argc, char **argv){
    int index;
    for (index=0; index<argc; index++){
        if (!strcmp(argv[index],"-m")){
            return atoi(argv[index+1]);
        }
    }
    return READ_NOTHING;
}

int read_quantum(int argc, char **argv){
    int index;
    for (index=0; index<argc; index++){
        if (!strcmp(argv[index],"-q")){
            return atoi(argv[index+1]);
        }
    }
    return READ_NOTHING;
}

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

detail_t *get_process(detail_t *root, int process_id){
    if (root->process_id == process_id){
        return root;
    }else{
        root->next = get_process(root, process_id);
    }
    return root;
}

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
    }
    return root;
}

void print_details_all(detail_t *root){
    // test function only
    detail_t *current = root;
    while (current!=NULL){
        printf("Create Time: %d. Process ID: %d. ",current->time_created,current->process_id);
        printf("Memory Size: %d. Job Time: %d.\n", current->memory_size, current->job_time);
        current = current->next;
    }
}

void print_queue_all(queue_t *root){
    // test function only
    queue_t *current = root;
    printf("--Queue Print Starts--\n");
    while (current!=NULL){
        printf("Process ID: %d. Executed Time: %d. Quantum Time: %d. Job Time: %d.\n",current->process->process_id,current->excuted_time,current->quantum_executed_time,current->process->job_time);
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

/*
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
