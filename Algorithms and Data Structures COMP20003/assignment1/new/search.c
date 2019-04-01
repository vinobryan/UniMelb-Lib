#include "header.h"

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
    int found_once = 0;
    Node *p = root, *last_p = NULL;
    while(p != NULL)
    {
        last_p = p;
        if(strcmp(key, p->name) < 0)
        {
            p = p->left;
            search_count++;
        }
        else if(strcmp(key, p->name) > 0)
        {
            p = p->right;
            search_count++;
        }
        else if(strcmp(key, p->name) == 0)
        {
            output_data(p->name, p->data, search_count, output_file, FOUND);
            found_once = 1;
            p = p->left;
        }
    }
    if (found_once == 0)
    {
        output_data(key, "", search_count, output_file, NOT_FOUND);
    }
    return 0;
}

/*
 * Output data into a file, adding data at the end of file
 * Parameters:
 * char data[]: The data that to be output
 * char file_name[]: The output file name
 */
int write_to_file(char data[], char file_name[])
{
    FILE *fp = fopen(file_name, "a+");
    if (fp==0)
    {
        printf("can't open file\n");
        return 0;
    }
    fseek(fp, 0, SEEK_END);
    fwrite(data, strlen(data), 1, fp);
    fclose(fp);
    return 0;
}

int output_data(char name[], char data[], int counter, char file_name[],
                int status)
{
    if (status == FOUND)
    {
        write_to_file(name, file_name);
        write_to_file(" --> ", file_name);
        write_to_file(data, file_name);
        write_to_file("\n", file_name);
        fprintf(stdout, "%s --> %d\n", name, counter);
    }
    else if (status == NOT_FOUND)
    {
        write_to_file(name, file_name);
        write_to_file(" --> NOT FOUND\n", file_name);
        fprintf(stdout, "%s --> %d\n", name, counter);
    }
}
