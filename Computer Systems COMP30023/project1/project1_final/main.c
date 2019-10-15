/**
 * Ziren Xiao
 * StudentID 675485
 */

#include "header.h"

/**
 * Main function
 */
int main(int argc, char **argv) {
    int quantum, memory_size;
    char *algorithm_name, *file_name;
    scheduler_t *scheduler = NULL;

    quantum = read_quantum(argc, argv);
    memory_size = read_memory_size(argc, argv);
    algorithm_name = read_algorithm_name(argc, argv);
    file_name = read_file_name(argc, argv);

    scheduler = construct_scheduler(scheduler, memory_size, file_name);
    scheduler = schedule(scheduler, quantum, algorithm_name, memory_size);
    return MAIN_RETURN;
}
