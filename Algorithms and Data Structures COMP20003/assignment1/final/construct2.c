/* construct2.c
 *
 * Created by Ziren Xiao 675485 (zirenx@student.unimelb.edu.au)
 * 05/09/2016
 *
 * Construct a tree (dictionary), including insert and search
 * operations
 *
 */

#include "header2.h"

/*
 * Construct the tree using given data from a file
 * Parameters:
 * char datafile[]: The file name of input file
 */
Node *construct_tree(char datafile[]){
    FILE *fp = NULL;
    Node *root;
    char line[MAX_LENGTH_RECORD];
    int first_line = 1;
    if ((fp = fopen(datafile, "at+")) != NULL){
        while (fgets(line, MAX_LENGTH_RECORD, fp) != NULL){
            char *name;
            char *data;
            name = strtok(line, SEMICONLON);
            data = strtok(NULL, SEMICONLON);
            if (first_line){
                root = makedict(name, data);
                first_line = 0;
            } else {
                root = insert(root, name, data);
            }
        }
        fclose(fp);
        fp = NULL;
    }
    return root;
}

/*
 * Insert the a new node into the tree
 * Parameters:
 * Node *root: The tree that you want to insert data
 * char name[]: The name of the node that to be inserted
 * char name[]: The data of the node
 */
Node *insert(Node *root, char name[], char data[]){
    if (root == NULL){
        if ((root = (Node*)malloc(sizeof(Node))) != NULL){
            strcpy(root->name, name);
            strcpy(root->data, data);
            root->left = NULL;
            root->right = NULL;
            root->head = NULL;
        }
    } else if (strcmp(name, root->name) < 0){
        root->left = insert(root->left, name, data);
    } else if (strcmp(name, root->name) > 0){
        root->right = insert(root->right, name, data);
    } else if (strcmp(name, root->name) == 0){
        root->head = duplicate_insert(root->head, name, data);
    }
    return root;
}

/*
 * Insert a duplicate key into list
 * Parameters:
 * List *head: The pointer of the list (or the head)
 * char name[]: The insert name
 * char data[]: The insert data
 */
List *duplicate_insert(List *head, char name[], char data[]){
    if (head != NULL){
        head->next = duplicate_insert(head->next, name, data);
    } else if ((head = (List*)malloc(sizeof(List))) != NULL){
        strcpy(head->name, name);
        strcpy(head->data, data);
        head->next = NULL;

    }
    return head;
}

/*
 * Search a duplicate key in a list
 * Parameters:
 * List *head: The pointer of the list (or the head)
 * int search_count: The number of searching time
 * char output_file[]: The output file name
 */
int duplicate_search(List *head, int search_count, char output_file[]){
    if (head != NULL){
        write_to_file(head->name, output_file);
        write_to_file(" --> ", output_file);
        write_to_file(head->data, output_file);
        write_to_file("\n", output_file);
        duplicate_search(head->next, search_count, output_file);
    }
    return 0;
}

/*
 * Locate the key in the tree
 * Parameters:
 * Node *root: The main tree
 * char output_file[]: The name of output file
 * char key[]: The searching key
 */
int search(Node *root, char output_file[], char key[]){
    int search_count = 0;
    int found_once = 0;
    Node *p = root, *last_p = NULL;
    while(p != NULL){
        last_p = p;
        if(strcmp(key, p->name) < 0){
            p = p->left;
            search_count++;
        } else if (strcmp(key, p->name) > 0){
            p = p->right;
            search_count++;
        } else if (strcmp(key, p->name) == 0){
            output_data(p->name, p->data, search_count, output_file, FOUND);
            duplicate_search(p->head, search_count, output_file);
            found_once = 1;
            search_count++;
            p = p->left;
        }
    }
    if (found_once == 0){
        output_data(key, "", search_count, output_file, NOT_FOUND);
    }
    return 0;
}

/*
 * Initialise the binary tree and input first name and data into root.
 * Parameters:
 * char name[]: first name in file
 * char data[]: first data in file
 */
Node *makedict(char name[], char data[]){
    Node *root;
    if ((root = (Node*)malloc(sizeof(Node))) != NULL){
        strcpy(root->name, name);
        strcpy(root->data, data);
        root->left = NULL;
        root->right = NULL;
        root->head = NULL;
    }
    return root;
}
