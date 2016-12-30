#include <Foundation/Foundation.h>

/**
 * CodinGame planet is being attacked by slimy insectoid aliens.
 * <---
 * Hint:To protect the planet, you can implement the pseudo-code provided in the statement, below the player.
 **/
int main(int argc, const char * argv[]) {

    // game loop
    while (1) {
        char enemy1[256]; // name of enemy 1
        scanf("%s", enemy1);
        int dist1; // distance to enemy 1
        scanf("%d", &dist1);
        char enemy2[256]; // name of enemy 2
        scanf("%s", enemy2);
        int dist2; // distance to enemy 2
        scanf("%d", &dist2);

        // Write an action using printf(). DON'T FORGET THE TRAILING NEWLINE \n
        // To debug: fprintf(stderr, [@"Debug messages\n" UTF8String]);


        // You have to output a correct ship name to shoot ("Buzz", enemy1, enemy2, ...)
        if (dist1 < dist2)
            printf("%s\n", enemy1);
        else
            printf("%s\n", enemy2);
    }
}