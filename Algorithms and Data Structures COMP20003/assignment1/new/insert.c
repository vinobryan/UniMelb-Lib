#include "header.h"

/*
 * Insert the a new node into the tree
 * Parameters:
 * Node *root: The tree that you want to insert data
 * char name[]: The name of the node that to be inserted
 * char name[]: The data of the node
 */
Node *insert(Node *root, char name[], char data[])
{
    if(root == NULL)
    {
        if ((root = (Node*)malloc(sizeof(Node)))!=NULL)
        {
            strcpy(root->name, name);
            strcpy(root->data, data);
            root->left = NULL;
            root->right = NULL;
            fprintf(stdout, "Insert ends:%s\n", name);
        }
    }
    else if(strcmp(name, root->name) <= 0)
    {
        root->left = insert(root->left, name, data);
    }
    else if(strcmp(name, root->name) > 0)
    {
        root->right = insert(root->right, name, data);
    }
    return root;
}
