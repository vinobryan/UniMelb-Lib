/* Exercise 3.6: Calculating change (using loop and array)
   Jianzhong Qi, September 2015.
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
#define SENTINEL -1

int
main(int argc, char *argv[]) {

	int amount;
	int coins[] = {COIN1, COIN2, COIN3, COIN4, COIN5, COIN6, SENTINEL};

	/* get the input value */
	printf("Enter amount in cents: ");
	if (scanf("%d", &amount)!=1 || (amount<0) || (amount>MAX_CHANGE)) {
		/* the if statement is introduced in Chapter 3 */
		printf("Error in input\n");
		exit(EXIT_FAILURE);
	}

	/* now try all the coins, one by one
	*/
	int i = 0;
	while (coins[i]!=SENTINEL && amount > 0) {
		if (amount>=coins[i]) {
			printf("give a %2dc coin\n", coins[i]);
			amount -= coins[i];
		} else {
			i++;
		}
	}

	/* and now a final check */
	printf("amount remaining: %dc\n", amount);
	if (amount!=0) {
		printf("Looks like something is wrong\n");
		exit(EXIT_FAILURE);
	}

	return 0;
}
