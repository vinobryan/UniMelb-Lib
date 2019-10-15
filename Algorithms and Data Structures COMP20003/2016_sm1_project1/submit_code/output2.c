/* output2.c
 *
 * Created by Ziren Xiao 675485 (zirenx@student.unimelb.edu.au)
 * 05/09/2016
 *
 * The function(s) that used to output data to a file
 *
 */

#include "header2.h"

/*
 * Output data into a file, adding data at the end of file
 * Parameters:
 * char data[]: The data that to be output
 * char file_name[]: The output file name
 */
int write_to_file(char data[], char file_name[]){
    FILE *fp = fopen(file_name, "a+");
    if (fp == FILE_OPEN_FAIL){
        printf("can't open file\n");
        return 0;
    }
    fseek(fp, WRITE_FILE_MOVE, SEEK_END);
    fwrite(data, strlen(data), TOTAL_NUMBER_CHAR, fp);
    fclose(fp);
    return 0;
}

/*
 * Output data into a file in specific content
 * Parameters:
 * char data[]: The data that to be output
 * char file_name[]: The output file name
 * int counter: The number of searching times
 * int status: To indicate whether the key is found or not
 */
int output_data(char name[], char data[], int counter, char file_name[],
                int status){
    if (status == FOUND){
        write_to_file(name, file_name);
        write_to_file(" --> ", file_name);
        write_to_file(data, file_name);
        write_to_file("\n", file_name);
        fprintf(stdout, "%s --> %d\n", name, counter);
    }else if (status == NOT_FOUND){
        write_to_file(name, file_name);
        write_to_file(" --> NOT FOUND\n", file_name);
        fprintf(stdout, "%s --> %d\n", name, counter);
    }
}
