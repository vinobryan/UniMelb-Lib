#include "header.h"

/*
 * Initialise the binary tree and input first name and data into root.
 * Parameters:
 * char name[]: first name in file
 * char data[]: first data in file
 */
Node *makedict(char name[], char data[])
{
    Node *root;
    if ((root = (Node*)malloc(sizeof(Node)))!=NULL)
    {
        strcpy(root->name, name);
        strcpy(root->data, data);
        root->left = NULL;
        root->right = NULL;
    }
    return root;
}
