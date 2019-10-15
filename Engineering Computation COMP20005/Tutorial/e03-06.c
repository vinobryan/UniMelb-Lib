/* Exercise 3.6: Calculating change
   This is an ugly program, and if arrays and loops are used it can be made
   much more elegant, see Chapter 7.
   Alistair Moffat, March 2013.
*/

#include <stdio.h>
#include <stdlib.h>

#define MAX_CHANGE 99

#define COIN1 50
#define COIN2 20
#define COIN3 10
#define COIN4 05
#define COIN5 02
#define COIN6 01

int
main(int argc, char *argv[]) {

	int amount;

	/* get the input value */
	printf("Enter amount in cents: ");
	if (scanf("%d", &amount)!=1 || (amount<0) || (amount>MAX_CHANGE)) {
		/* the if statement is introduced in Chapter 3 */
		printf("Error in input\n");
		exit(EXIT_FAILURE);
	}

	/* now try all the coins, one by one, noting that some coins have
	   to be tried twice, and so for simplicity, this program in fact
	   tries every coin twice. Note that highly repetitive code is a
	   sure sign that there is a better way!
	*/
	if (amount>=COIN1) {
		printf("give a %2dc coin\n", COIN1);
		amount -= COIN1;
	}
	if (amount>=COIN1) {
		printf("give a %2dc coin\n", COIN1);
		amount -= COIN1;
	}
	if (amount>=COIN2) {
		printf("give a %2dc coin\n", COIN2);
		amount -= COIN2;
	}
	if (amount>=COIN2) {
		printf("give a %2dc coin\n", COIN2);
		amount -= COIN2;
	}
	if (amount>=COIN3) {
		printf("give a %2dc coin\n", COIN3);
		amount -= COIN3;
	}
	if (amount>=COIN3) {
		printf("give a %2dc coin\n", COIN3);
		amount -= COIN3;
	}
	if (amount>=COIN4) {
		printf("give a %2dc coin\n", COIN4);
		amount -= COIN4;
	}
	if (amount>=COIN4) {
		printf("give a %2dc coin\n", COIN4);
		amount -= COIN4;
	}
	if (amount>=COIN5) {
		printf("give a %2dc coin\n", COIN5);
		amount -= COIN5;
	}
	if (amount>=COIN5) {
		printf("give a %2dc coin\n", COIN5);
		amount -= COIN5;
	}
	if (amount>=COIN6) {
		printf("give a %2dc coin\n", COIN6);
		amount -= COIN6;
	}
	if (amount>=COIN6) {
		printf("give a %2dc coin\n", COIN6);
		amount -= COIN6;
	}

	/* and now a final check */
	printf("amount remaining: %dc\n", amount);
	if (amount!=0) {
		printf("Looks like something is wrong\n");
		exit(EXIT_FAILURE);
	}

	return 0;
}
