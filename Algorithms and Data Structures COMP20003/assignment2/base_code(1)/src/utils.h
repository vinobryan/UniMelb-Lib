#ifndef __UTILS__
#define __UTILS__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <stdbool.h>
#include <stdint.h>
#include <time.h>
#include <signal.h>


#define SIZE 4
#define _XOPEN_SOURCE 500

/**
* Move type
*/
typedef enum moves{
	left=0,
	right=1,
	up=2,
	down=3
} move_t;

/**
 * Back Propagation type 
 */
typedef enum propagation{
	max=0,
	avg=1
} propagation_t;


/**
 * Renders the board
 */
void drawBoard(uint8_t board[SIZE][SIZE], uint32_t score);


/**
 * Updates the board with each action, and updates the score
 */
bool moveUp(uint8_t board[SIZE][SIZE], uint32_t* score);
bool moveLeft(uint8_t board[SIZE][SIZE], uint32_t* score);
bool moveDown(uint8_t board[SIZE][SIZE], uint32_t* score);
bool moveRight(uint8_t board[SIZE][SIZE], uint32_t* score);

/**
 * Checks if game has ended
 */
bool gameEnded(uint8_t board[SIZE][SIZE]);

/**
* Counts the number of empty cells
*/
uint8_t countEmpty(uint8_t board[SIZE][SIZE]);

/**
* Given an index, returns the tile value, i is row, j is column
*/
uint32_t getTile( uint8_t board[SIZE][SIZE], unsigned i, unsigned j);

/**
* initial random board, and a function to randmoly add a tile
*/
void initBoard(uint8_t board[SIZE][SIZE], uint32_t* score);
void addRandom(uint8_t board[SIZE][SIZE]);

/**
 * Executes an action, updates the board and the score, and return true if the board has changed,
 * and false otherwise
 */
bool execute_move_t( uint8_t  board[SIZE][SIZE], uint32_t* score, move_t move);

#endif
