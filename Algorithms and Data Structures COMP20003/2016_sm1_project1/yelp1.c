#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node
{
    char name[100];
    char data[1465];
    struct node *left;
    struct node *right;
}Node;

int main(int argc, char *argv[])
{
    char *data_file = argv[1];
    char *output_file = argv[2];
    char *key_file = argv[3];
    Node *root = NULL;
    Construct_Tree(data_file, *root);
    Find_Key(*root, key_file, output_file);
    return 0;
}

/*
 Search the key in a binary tree and output results to two files, return an integer that indicates if data found, 0 for not found, 1 for found
 Parameters:
   Node **root: The root of the tree
   char key[]: The key (name) that to be searched
   char output_file[]: The name of output file
*/
int Find_Key(Node *root, char *key, char *output_file)
{
    int search_count = 0;
    Node *p = root, *last_p = NULL;
    while(p!=NULL)
    {
        last_p = p;
        if(strcmp(p->name, key) < 0)
        {
            p = p->left;
            search_count++;
        }
        else if(strcmp(p->name, key) > 0)
        {
            p = p->right;
            search_count++;
        }
        else if(strcmp(p->name, key) == 0)
        {
            char output_data[] = "";
            strcat(output_data, p->name);
            strcat(output_data, " --> ");
            strcat(output_data, p->data);
            Write_In_File(output_data, output_file);
            strcat(output_data, p->name);
            strcat(output_data, " --> ");
            strcat(output_data, search_count);
            Write_In_File(output_data, "stdout");
            return 1;
        }else
        {

        }
    }
    if (p == NULL)
    {
        return 0;
    }
}


void Write_In_File(char *data, char *file_name)
{
    FILE *fp;
    fclose(fp);
    if((fp=fopen(*file_name,"at+"))==NULL)
    {
        printf("Cannot open file!");
        exit(1);
    }
    fputs(*data, fp);
    rewind(fp);
    fclose(fp);
}

/*
 Import the content in file into program and then construct a binary tree
 Parameters:
   char datafile[]: The csv file that to be imported
   Node **root: The root of the tree
*/
Node *Construct_Tree(char datafile[])
{
    FILE *fp = NULL;
    Node *root;

    char line[Max_length_record];
    int first_line = 1;
    if((fp = fopen(datafile, "at+")) != NULL)
    {
        char *semicolon = ",";
        while (fgets(line, Max_length_record, fp)!=NULL)
        {
            char *name;
            name = strtok(line, semicolon);
            char *data;
            data = strtok(NULL, semicolon);
            if(first_line){
                root = Init_Binary_Tree(name, data);
                first_line = 0;
            }
            else
            {
                root = Insert_Node(root, name, data);
            }
        }
        fclose(fp);
        fp = NULL;
    }
    return root;
}


/*
 Initialise a binary tree, return whole tree
 Parameters:
   char first_name[]: The first name in file that to be interted
   char fitst_data[]: The first data in corresponding to the first name that to be inserted
*/
Node *Init_Binary_Tree(char first_name[], char first_data[])
{
    Node *root;
    if ((root = (Node*)malloc(sizeof(Node)))!=NULL)
    {
        strcpy(root->name, first_name);
        strcpy(root->data, first_data);
        root->left=NULL;
        root->right=NULL;
    }
    return root;
}

/*
 Insert a node into the tree, return whole tree
 Parameters:
   Node *root: The root of the tree
   char name[]: The name that to be interted
   char data[]: The data corresponding to the name that to be inserted
*/
Node *Insert_Node(Node *root, char name[], char data[])
{
    Node *newnode;
    Node *p = root, *last_p = NULL;
    if ((newnode = (Node*)malloc(sizeof(Node)))!=NULL)
    {
    strcpy(newnode->name, name);
    strcpy(newnode->data, data);
    newnode->left = NULL;
    newnode->right = NULL;
    }

    while(p!=NULL)
    {
        last_p = p;
        if(strcmp(newnode->name, p->name) < 0)
        {
            p = p->left;
        }
        else if(strcmp(newnode->name, p->name) > 0)
        {
            p = p->right;
        }
        else
        {
            printf("Name to be inserted has existed.\n");
            exit(1);
        }
    }
    p = last_p;
    if(strcmp(newnode->name, p->name) < 0)
    {
        p->left = newnode;
        printf("Insert success! Test:Left->%s\n", p->left->name);
    }
    else
    {
        p->right = newnode;
        printf("Insert success! Test:Right->%s\n", p->right->name);
    }
    return root;
}
