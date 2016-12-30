using System;
using System.Linq;
using System.IO;
using System.Text;
using System.Collections;
using System.Collections.Generic;

/**
 * CodinGame planet is being attacked by slimy insectoid aliens.
 * <---
 * Hint:To protect the planet, you can implement the pseudo-code provided in the statement, below the player.
 **/
class Player
{
    static void Main(string[] args)
    {

        // game loop
        while (true)
        {
            string enemy1 = Console.ReadLine(); // name of enemy 1
            int dist1 = int.Parse(Console.ReadLine()); // distance to enemy 1
            string enemy2 = Console.ReadLine(); // name of enemy 2
            int dist2 = int.Parse(Console.ReadLine()); // distance to enemy 2

            // Write an action using Console.WriteLine()
            // To debug: Console.Error.WriteLine("Debug messages...");


            // You have to output a correct ship name to shoot ("Buzz", enemy1, enemy2, ...)
            if (dist1 < dist2)
                Console.WriteLine(enemy1);
            else
                Console.WriteLine(enemy2);
        }
    }
}