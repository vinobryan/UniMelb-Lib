/* yelp1.c
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

#include "tree.h"

/*
 * Main function
 */
int main(int argc, char *argv[])
{
    /* Declare all non-void functions here */
    Node *Init_Binary_Tree(char first_name[], char first_data[]);
    Node *insert(Node *root, char name[], char data[]);
    Node *construct_tree(char datafile[]);
    Samekey *insert_duplicate(Samekey *head, char name[], char data[]);
    /* Declare function ends */
    Node *root;
    char *input_data = argv[1];
    char *output_file = argv[2];
    root = construct_tree(input_data);
    printf("-------------Find Key Start Here-------------\n");
    read_keys(root, output_file);
    return 0;
}



/*
 * Insert duplicate key into a linked list
 * Parameters:
 * Samekey *head: The head of duplicate key
 * char name[]: The name of duplicate key
 * char data[]: The data of duplicate key
 */
Samekey *insert_duplicate(Samekey *head, char name[], char data[])
{
    Samekey *current;
    if (head == NULL)
    {
        head = (Samekey*)malloc(sizeof(Samekey));
        strcpy(head->name, name);
        strcpy(head->data, data);
        head->next = NULL;
        printf("Same Key Name:%s\n", current->name);
    }
    else
    {
        current = head->next;
        while (current!=NULL)
        {
            current = current->next;
        }
        if ((current = (Samekey*)malloc(sizeof(Samekey)))!=NULL)
        {
            strcpy(current->name, name);
            strcpy(current->data, data);
            current->next = NULL;
            printf("Same Key Name:%s\n", current->name);
        }
    }
    return head;

}

/*
 * Insert the a new node into the tree
 * Parameters:
 * Node *root: The main tree
 * char name[]: The name of the node that to be inserted
 * char name[]: The data of the node
 */
Node *insert(Node *root, char name[], char data[])
{
    Node *newnode;
    Node *p = root, *last_p = NULL;
    if ((newnode = (Node*)malloc(sizeof(Node)))!=NULL)
    {
        strcpy(newnode->name, name);
        strcpy(newnode->data, data);
        newnode->left = NULL;
        newnode->right = NULL;
        newnode->head=NULL;
    }
    printf("Insert Route:\n");
    while(p!=NULL)
    {

        last_p = p;
        if(strcmp(newnode->name, p->name) < 0)
        {
            printf("Left -> ");
            p = p->left;
        }
        else if(strcmp(newnode->name, p->name) > 0)
        {
            printf("Right -> ");
            p = p->right;
        }
        else
        {
            break;
        }
    }
    printf("End.\n");
    p = last_p;
    if(strcmp(newnode->name, p->name) < 0)
    {
        p->left = newnode;
        printf("Insert success! Name:%s\n", p->left->name);
        printf("-------------------------\n");
    }
    else if(strcmp(newnode->name, p->name) > 0)
    {
        p->right = newnode;
        printf("Insert success! Name:->%s\n", p->right->name);
        printf("-------------------------\n");
    }else{
        int i=0;
        p->head = insert_duplicate(p->head, name, data);
        printf("Insert Same Key success! \n");
        printf("-------------------------\n");
    }
    return root;
}

/*
 * Construct the tree using given data from a file
 * Parameters:
 * char datafile[]: The file name of input file
 */
Node *construct_tree(char datafile[])
{
    FILE *fp = NULL;
    Node *root;

    char line[max_length_record];
    int first_line = 1;
    if((fp = fopen(datafile, "at+")) != NULL)
    {
        char *semicolon = ",";
        while (fgets(line, max_length_record, fp)!=NULL)
        {
            char *name;
            name = strtok(line, semicolon);
            char *data;
            data = strtok(NULL, semicolon);
            if(first_line){
                root = makedict(name, data);
                first_line = 0;
            }
            else
            {
                root = insert(root, name, data);
            }
        }
        fclose(fp);
        fp = NULL;
    }
    return root;
}

/*
 * Read searching keys from user input or file
 * Parameters:
 * Node *root: The main tree
 * char output_file[]: The name of output file
 */
int read_keys(Node *root, char output_file[])
{
    char key[64];
    while (scanf("%s", key) == 1)
    {
        search(root, output_file, key);
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
int search(Node *root, char output_file[], char key[])
{
    int search_count = 0;
    Node *p = root, *last_p = NULL;
    printf("----------------Find Key Starts Here----------------\n");
    while(p!=NULL)
    {
        last_p = p;
        if(strcmp(key, p->name) < 0)
        {
            printf("Goes Left, Name:%s, Key:%s, Compare:%d\n",p->name,key,strcmp(p->name, key));
            p = p->left;
            search_count++;
        }
        else if(strcmp(key, p->name) > 0)
        {
            printf("Goes Right, Name:%s, Key:%s, Compare:%d\n",p->name,key,strcmp(p->name, key));
            p = p->right;
            search_count++;
        }
        else if(strcmp(key, p->name) == 0)
        {
            printf("Found it\n");
            char search_count_data[] = "";
            write_to_file(p->name, output_file);
            write_to_file(" --> ", output_file);
            write_to_file(p->data, output_file);
            write_to_file("\n", output_file);
            if (p->head!=NULL)
            {
                print_list(p->head, output_file);
            }
            sprintf(search_count_data, "%d", search_count);
            fprintf(stdout, "%s -> %s\n", key, search_count_data);
            printf("Search count:%s\n",search_count_data);
            return 0;
        }
    }
    if (p == NULL)
    {
        char search_count_data[] = "";
        sprintf(search_count_data, "%d", search_count);
        write_to_file(key, output_file);
        write_to_file(" --> ", output_file);
        write_to_file("NOT FOUND", output_file);
        write_to_file("\n", output_file);
        fprintf(stdout, "%s -> %s\n", key, search_count_data);
        return 0;
    }
}

/*
 * Output all data in the linked list to a file
 * Parameters:
 * Samekey *head: The head of linked list
 * char output_file[]: The file name of output
 */
int print_list(Samekey *head, char output_file[])
{
    Samekey *current;
    current = head;
    while (current!=NULL)
    {
        write_to_file(current->name, output_file);
        write_to_file(" --> ", output_file);
        write_to_file(current->data, output_file);
        write_to_file("\n", output_file);
        current = current->next;
    }
    return 0;
}
