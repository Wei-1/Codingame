import 'dart:io';
import 'dart:math';

/**
 * CodinGame planet is being attacked by slimy insectoid aliens.
 * <---
 * Hint:To protect the planet, you can implement the pseudo-code provided in the statement, below the player.
 **/
void main() {

    // game loop
    while (true) {
        String enemy1 = stdin.readLineSync(); // name of enemy 1
        int dist1 = int.parse(stdin.readLineSync()); // distance to enemy 1
        String enemy2 = stdin.readLineSync(); // name of enemy 2
        int dist2 = int.parse(stdin.readLineSync()); // distance to enemy 2

        // Write an action using print()
        // To debug: stderr.writeln('Debug messages...');

        // You have to output a correct ship name to shoot ("Buzz", enemy1, enemy2, ...)
        if (dist1 < dist2)
            print(enemy1);
        else
            print(enemy2);
    }
}