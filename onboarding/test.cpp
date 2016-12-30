#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

/**
 * CodinGame planet is being attacked by slimy insectoid aliens.
 * <---
 * Hint:To protect the planet, you can implement the pseudo-code provided in the statement, below the player.
 **/
int main()
{

    // game loop
    while (1) {
        string enemy1; // name of enemy 1
        cin >> enemy1; cin.ignore();
        int dist1; // distance to enemy 1
        cin >> dist1; cin.ignore();
        string enemy2; // name of enemy 2
        cin >> enemy2; cin.ignore();
        int dist2; // distance to enemy 2
        cin >> dist2; cin.ignore();

        // Write an action using cout. DON'T FORGET THE "<< endl"
        // To debug: cerr << "Debug messages..." << endl;


        // You have to output a correct ship name to shoot ("Buzz", enemy1, enemy2, ...)
        if (dist1 < dist2)
            cout << enemy1 << endl;
        else
            cout << enemy2 << endl;
    }
}