
/*
 * Read searching keys from user input or file
 * Parameters:
 * Node *root: The main tree
 * char output_file[]: The name of output file
 */
int read_keys(Node *root, char output_file[])
{
    char key[MAX_LENGTH_NAME];
    while (scanf("%s", key) == 1)
    {
        search(root, output_file, key);
    }
    return 0;
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
    char line[MAX_LENGTH_RECORD];
    int first_line = 1;
    if((fp = fopen(datafile, "at+")) != NULL)
    {
        while (fgets(line, MAX_LENGTH_RECORD, fp)!=NULL)
        {
            char *name;
            char *data;
            name = strtok(line, SEMICONLON);
            data = strtok(NULL, SEMICONLON);
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
