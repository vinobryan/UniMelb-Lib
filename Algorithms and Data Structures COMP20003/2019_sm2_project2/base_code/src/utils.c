#include <curses.h>
#include "utils.h"
#include "pacman.h"

bool execute_move_t(state_t* state, move_t move) {

    int ch;                         //Key pushed
    static int chtmp;               //Buffered key
    int tmp;
	bool changed_direction = false;
    ch = getch();                   //Figure out which key is pressed

    //If they are not pressing something, use previous input
    if(ch == ERR) ch = chtmp;
    chtmp = ch;

    //Determine which button is pushed
    switch (move) {
    case up:          //Move pacman up
        if(state->Loc[4][0] <= 0) tmp = LEVEL_HEIGHT - 1;
        else tmp = state->Loc[4][0] - 1;
        if((state->Level[tmp][state->Loc[4][1]] != 1)
        && (state->Level[tmp][state->Loc[4][1]] != 4))
            { state->Dir[4][0] = -1; state->Dir[4][1] =  0; changed_direction = true; }
        break;

    case down:         //Move pacman down
        if(state->Loc[4][0] >= 28) tmp = 0;
        else tmp = state->Loc[4][0] + 1;
        if((state->Level[tmp][state->Loc[4][1]] != 1)
        && (state->Level[tmp][state->Loc[4][1]] != 4))
            { state->Dir[4][0] =  1; state->Dir[4][1] =  0; changed_direction = true; }
        break;

    case left:         //Move pacman left
        if(state->Loc[4][1] <= 0) tmp = LEVEL_WIDTH - 1;
        else tmp = state->Loc[4][1] - 1;
        if((state->Level[state->Loc[4][0]][tmp] != 1)
        && (state->Level[state->Loc[4][0]][tmp] != 4))
            { state->Dir[4][0] =  0; state->Dir[4][1] = -1; changed_direction = true;}
        break;

    case right:          //Move pacman right
        if(state->Loc[4][1] >= 27) tmp = 0;
        else tmp = state->Loc[4][1] + 1;
        if((state->Level[state->Loc[4][0]][tmp] != 1)
        && (state->Level[state->Loc[4][0]][tmp] != 4))
            { state->Dir[4][0] =  0; state->Dir[4][1] =  1; changed_direction = true;}
        break;

    

    }

	MovePacmanSim(state);    CheckCollisionSim(state);
    MoveGhostsSim(state);    CheckCollisionSim(state);


	return changed_direction;

	
}

/****************************************************************
* Function:    CheckCollision(state_t* state)                                 *
* Parameters:  none                                             *
* Returns:     none                                             *
* Description: Check and handle if Pacman collided with a ghost *
****************************************************************/
void CheckCollisionSim(state_t* state) {

    //Temporary variable
    int a = 0;

    //Check each ghost, one at a time for collision
    for(a = 0; a < 4; a++) {

        //Ghost X and Y location is equal to Pacman X and Y location (collision)
        if((state->Loc[a][0] == state->Loc[4][0]) && (state->Loc[a][1] == state->Loc[4][1])) {

            //Pacman is invincible, ghost dies
            if(state->Invincible == 1) {

                state->Points = state->Points + state->GhostsInARow * 20;    //Increase points by an increasing value

                
                state->GhostsInARow *= 2;               

                //Reset the ghost's position to the starting location
                state->Loc[a][0] = state->StartingPoints[a][0]; state->Loc[a][1] = state->StartingPoints[a][1];
            }

            //Pacman is vulnerable, Pacman dies
            else {        

                //Subtract one life 
                state->Lives--;

                //Reset Pacman's and ghosts' positions
                for(a = 0; a < 5; a++) {
                    state->Loc[a][0] = state->StartingPoints[a][0];
                    state->Loc[a][1] = state->StartingPoints[a][1];
                }

                //Reset directions
                state->Dir[0][0] =  1; state->Dir[0][1] =  0;
                state->Dir[1][0] = -1; state->Dir[1][1] =  0;
                state->Dir[2][0] =  0; state->Dir[2][1] = -1;
                state->Dir[3][0] =  0; state->Dir[3][1] =  1;
                state->Dir[4][0] =  0; state->Dir[4][1] = -1;
                
            }
        }
    }
}


/****************************************************************
* Function:    MoveGhosts()                                     *
* Parameters:  none                                             *
* Returns:     none                                             *
* Description: Move all ghosts and check for wall collisions    *
****************************************************************/
void MoveGhostsSim(state_t* state) {

    //Set up some temporary variables
    int a = 0; int b = 0; int c = 0;
    int checksides[] = { 0, 0, 0, 0, 0, 0 };
    static int SlowerGhosts = 0;
    int tmp;

    /* Move ghosts slower when they are vulnerable. This will allow
    the ghosts to move only x times for every y times Pacman moves */
    if(state->Invincible == 1) {
        SlowerGhosts++;
        if(SlowerGhosts > HOW_SLOW)
            SlowerGhosts = 0;
    }

    //If ghosts are not vulnerable OR the ghosts can move now
    if((state->Invincible == 0) || SlowerGhosts < HOW_SLOW)

    //Loop through each ghost, one at a time
    for(a = 0; a < 4; a++) {

        //Switch sides? (Transport to other side of screen)
             if((state->Loc[a][0] ==  0) && (state->Dir[a][0] == -1)) state->Loc[a][0] = 28;
        else if((state->Loc[a][0] == 28) && (state->Dir[a][0] ==  1)) state->Loc[a][0] =  0;
        else if((state->Loc[a][1] ==  0) && (state->Dir[a][1] == -1)) state->Loc[a][1] = 27;
        else if((state->Loc[a][1] == 27) && (state->Dir[a][1] ==  1)) state->Loc[a][1] =  0;
        else {

        //Determine which directions we can go
        for(b = 0; b < 4; b++) checksides[b] = 0;
        if(state->Loc[a][0] == 28) tmp = 0;
        else tmp = state->Loc[a][0] + 1;
        if(state->Level[tmp][state->Loc[a][1]] != 1) checksides[0] = 1;
        if(state->Loc[a][0] == 0) tmp = 28;
        else tmp = state->Loc[a][0] - 1;
        if(state->Level[tmp][state->Loc[a][1]] != 1) checksides[1] = 1;
        if(state->Loc[a][1] == 27) tmp = 0;
        else tmp = state->Loc[a][1] + 1;
        if(state->Level[state->Loc[a][0]][tmp] != 1) checksides[2] = 1;
        if(state->Loc[a][1] == 0) tmp = 27;
        else tmp = state->Loc[a][1] - 1;
        if(state->Level[state->Loc[a][0]][tmp] != 1) checksides[3] = 1;

        //Don't do 180 unless we have to
        c = 0; for(b = 0; b < 4; b++) if(checksides[b] == 1) c++;
        if(c > 1) {
                 if(state->Dir[a][0] ==  1) checksides[1] = 0;
            else if(state->Dir[a][0] == -1) checksides[0] = 0;
            else if(state->Dir[a][1] ==  1) checksides[3] = 0;
            else if(state->Dir[a][1] == -1) checksides[2] = 0;
        }

        c = 0;
        do {
            //Decide direction, based somewhat-randomly
            b = (int)(rand() / (1625000000 / 4));

            /* Tend to mostly chase Pacman if he is vulnerable
            or run away when he is invincible */
            if(checksides[b] == 1) {
                     if(b == 0) { state->Dir[a][0] =  1; state->Dir[a][1] =  0; }
                else if(b == 1) { state->Dir[a][0] = -1; state->Dir[a][1] =  0; }
                else if(b == 2) { state->Dir[a][0] =  0; state->Dir[a][1] =  1; }
                else if(b == 3) { state->Dir[a][0] =  0; state->Dir[a][1] = -1; }
            }
            else {
                if(state->Invincible == 0) {
                //Chase Pacman
                     if((state->Loc[4][0] > state->Loc[a][0]) && (checksides[0] == 1)) { state->Dir[a][0] =  1; state->Dir[a][1] =  0; c = 1; }
                else if((state->Loc[4][0] < state->Loc[a][0]) && (checksides[1] == 1)) { state->Dir[a][0] = -1; state->Dir[a][1] =  0; c = 1; }
                else if((state->Loc[4][1] > state->Loc[a][1]) && (checksides[2] == 1)) { state->Dir[a][0] =  0; state->Dir[a][1] =  1; c = 1; }
                else if((state->Loc[4][1] < state->Loc[a][1]) && (checksides[3] == 1)) { state->Dir[a][0] =  0; state->Dir[a][1] = -1; c = 1; }
                }

                else {
                //Run away from Pacman
                     if((state->Loc[4][0] > state->Loc[a][0]) && (checksides[1] == 1)) { state->Dir[a][0] = -1; state->Dir[a][1] =  0; c = 1; }
                else if((state->Loc[4][0] < state->Loc[a][0]) && (checksides[0] == 1)) { state->Dir[a][0] =  1; state->Dir[a][1] =  0; c = 1; }
                else if((state->Loc[4][1] > state->Loc[a][1]) && (checksides[3] == 1)) { state->Dir[a][0] =  0; state->Dir[a][1] = -1; c = 1; }
                else if((state->Loc[4][1] < state->Loc[a][1]) && (checksides[2] == 1)) { state->Dir[a][0] =  0; state->Dir[a][1] =  1; c = 1; }
                }
            }

        } while ((checksides[b] == 0) && (c == 0));

        //Move Ghost
        state->Loc[a][0] += state->Dir[a][0];
        state->Loc[a][1] += state->Dir[a][1];
        }
    }
}

/****************************************************************
* Function:    MovePacman()                                     *
* Parameters:  none                                             *
* Returns:     none                                             *
* Description: Move Pacman and check for wall collisions        *
****************************************************************/
void MovePacmanSim(state_t* state) {

    static int itime = 0;

    //Switch sides? (Transport to other side of screen)
         if((state->Loc[4][0] ==  0) && (state->Dir[4][0] == -1)) state->Loc[4][0] = 28;
    else if((state->Loc[4][0] == 28) && (state->Dir[4][0] ==  1)) state->Loc[4][0] =  0;
    else if((state->Loc[4][1] ==  0) && (state->Dir[4][1] == -1)) state->Loc[4][1] = 27;
    else if((state->Loc[4][1] == 27) && (state->Dir[4][1] ==  1)) state->Loc[4][1] =  0;

    //Or
    else {
        //Move Pacman
        state->Loc[4][0] += state->Dir[4][0];
        state->Loc[4][1] += state->Dir[4][1];

        //If he hit a wall, move back
        if((state->Level[state->Loc[4][0]][state->Loc[4][1]] == 1) || (state->Level[state->Loc[4][0]][state->Loc[4][1]] == 4)) {
            state->Loc[4][0] -= state->Dir[4][0];    state->Loc[4][1] -= state->Dir[4][1];
        }
    }

    //What is Pacman eating?
    switch (state->Level[state->Loc[4][0]][state->Loc[4][1]]) {
        case 2:    //Pellet
            state->Level[state->Loc[4][0]][state->Loc[4][1]] = 0;
            state->Points++;
            state->Food--;
            break;
        case 3:    //PowerUp
            state->Level[state->Loc[4][0]][state->Loc[4][1]] = 0;
            state->Invincible = 1;
            if(state->GhostsInARow == 0) state->GhostsInARow = 1;
            itime = time(0);
            break;
    }

    //Is he invincible?
    if(state->Invincible == 1)  state->tleft = (11 - state->LevelNumber - time(0) + itime);

    //Is invincibility up yet?
    if(state->tleft < 0) { state->Invincible = 0; state->GhostsInARow = 0; state->tleft = 0; }

}