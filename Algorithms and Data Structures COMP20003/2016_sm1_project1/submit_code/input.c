/* input.c
 *
 * Created by Ziren Xiao 675485 (zirenx@student.unimelb.edu.au)
 * 05/09/2016
 *
 * The function(s) that used to receive data from stdin
 *
 */

#include "header.h"

/*
 * Read searching keys from user input or file
 * Parameters:
 * Node *root: The main tree
 * char output_file[]: The name of output file
 */
int read_keys(Node *root, char output_file[]){
    char key[MAX_LENGTH_NAME];
    char temp_char;
    int key_counter = 0;
    fprintf(stdout, "Please type the searching keys now\n");
    while ((temp_char = getchar()) != INPUT_CHAR_ERROR){
        if (temp_char!='\n'){
            key[key_counter] = temp_char;
            key_counter++;
        } else {
            key[key_counter] = '\0';
            search(root, output_file, key);
            key_counter = 0;
        }
    }
    return 0;
}
