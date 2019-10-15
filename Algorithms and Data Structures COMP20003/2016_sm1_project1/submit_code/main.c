/* main.c
 *
 * Created by Ziren Xiao 675485 (zirenx@student.unimelb.edu.au)
 * 05/09/2016
 *
 * Input a csv file and a key (or keys), then construct a binary
 * tree and output searching result of the key in that file.
 *
 * To run the program type :
 * ./yelp1 input_file output_file < keys_file
 * Or
 * ./yelp1 input_file output_file, then type keys
 *
 */

#include "header.h"

int main(int argc, char *argv[]){
    Node *root;
    char *input_data = argv[1];
    char *output_file = argv[2];
    root = construct_tree(input_data);
    read_keys(root, output_file);
    return 0;
}
