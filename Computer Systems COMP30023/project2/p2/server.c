/* A simple server in the internet domain using TCP
The port number is passed as an argument


 To compile: gcc server.c -o server
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <strings.h>
#include <unistd.h>
#include <pthread.h>
#include <stdbool.h>

#define EXIT_CODE 1
#define ARG_NUMBER 2
#define BUFFER_SIZE 256
#define MAX_CLIENT_NUMBER 100
#define MAX_PENDING_JOB 10

struct client_detail{
    struct sockaddr_in cli_addr;
    int sockfd_client;
};

int init_arg(int argc, char **argv);
void socket_process(int argc, char **argv);
int read_msg(int sockfd_client, char buffer[]);
void write_msg(int sockfd_client, char *msg);
int accept_client(int sockfd, struct sockaddr_in cli_addr, socklen_t clilen);
void *client_thread(void *p_para);


int main(int argc, char **argv)
{
    socket_process(argc, argv);
}

int init_arg(int argc, char **argv){
    if (argc < ARG_NUMBER)
	{
		fprintf(stderr,"ERROR, no port provided\n");
		exit(EXIT_CODE);
	}
	return atoi(argv[1]);
}

int read_msg(int sockfd_client, char buffer[]){
    int number_bytes;
    /* Read characters from the connection,
    then process */
    bzero(buffer, BUFFER_SIZE);
    number_bytes = read(sockfd_client, buffer, BUFFER_SIZE-1);
	if (number_bytes < 0)
	{
		perror("ERROR reading from socket");
		exit(EXIT_CODE);
	}
	return number_bytes;
}

void write_msg(int sockfd_client, char *msg){
	if (write(sockfd_client,msg,sizeof(msg)) < 0)
	{
		perror("ERROR writing to socket");
		exit(EXIT_CODE);
	}
}

void *client_thread(void *p_para){
    struct client_detail *arg = (struct client_detail *)p_para;
    struct sockaddr_in cli_addr = arg->cli_addr;
    int sockfd_client = arg->sockfd_client;
    char buffer[BUFFER_SIZE];
    while (true){
        if (read_msg(sockfd_client, buffer)==0){
            break;
        }
        printf("Message from Client <%s> ID <%d>: %s\n",inet_ntoa(cli_addr.sin_addr), sockfd_client,buffer);
        write_msg(sockfd_client, "Received!\n");
    }
    printf("Client <%s> ID <%d> Closed.\n", inet_ntoa(cli_addr.sin_addr), sockfd_client);
    /* close socket */
	close(sockfd_client);
	return (void *)0;
}

void socket_process(int argc, char **argv){
	int sockfd, sockfd_client, portno;
    socklen_t clilen;
	struct sockaddr_in serv_addr, cli_addr;
	pthread_t pthread;
	struct client_detail p_para;

	 /* Create TCP socket */
    portno = init_arg(argc, argv);
    printf("Server Start.\n");
	sockfd = socket(AF_INET, SOCK_STREAM, 0);

	if (sockfd < 0)
	{
		perror("ERROR opening socket");
		exit(EXIT_CODE);
	}

	bzero((char *) &serv_addr, sizeof(serv_addr));

	/* Create address we're going to listen on (given port number)
	 - converted to network byte order & any IP address for
	 this machine */

	serv_addr.sin_family = AF_INET;
	serv_addr.sin_addr.s_addr = INADDR_ANY;
	serv_addr.sin_port = htons(portno);  /* store in machine-neutral format */

	 /* Bind address to the socket */

	if (bind(sockfd, (struct sockaddr *) &serv_addr,
			sizeof(serv_addr)) < 0)
	{
		perror("ERROR on binding");
		exit(EXIT_CODE);
	}

	/* Listen on socket - means we're ready to accept connections -
	 incoming connection requests will be queued */

	listen(sockfd, MAX_PENDING_JOB);

	clilen = sizeof(cli_addr);


    while(true){
        sockfd_client = accept(	sockfd, (struct sockaddr *) &cli_addr,
                            &clilen);
        if (sockfd_client < 0)
        {
            perror("ERROR on accept");
            exit(EXIT_CODE);
        }
        printf("Client <%s> Connected, assigned ID <%d>.\n", inet_ntoa(cli_addr.sin_addr), sockfd_client);
        p_para.cli_addr = cli_addr;
        p_para.sockfd_client = sockfd_client;
        if(pthread_create(&pthread,NULL,client_thread,&p_para)!=0){
            printf("Client <%d> failed!\n",sockfd_client);
            exit(1);
        }
    }
}
