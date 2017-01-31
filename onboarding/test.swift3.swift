import Glibc

public struct StderrOutputStream: TextOutputStream {
    public mutating func write(_ string: String) { fputs(string, stderr) }
}
public var errStream = StderrOutputStream()

/**
 * CodinGame planet is being attacked by slimy insectoid aliens.
 * <---
 * Hint:To protect the planet, you can implement the pseudo-code provided in the statement, below the player.
 **/


// game loop
while true {
    let enemy1 = readLine()! // name of enemy 1
    let dist1 = Int(readLine()!)! // distance to enemy 1
    let enemy2 = readLine()! // name of enemy 2
    let dist2 = Int(readLine()!)! // distance to enemy 2

    // Write an action using print("message...")
    // To debug: print("Debug messages...", to: &errStream)


    // You have to output a correct ship name to shoot ("Buzz", enemy1, enemy2, ...)
    if (dist1 < dist2) {
        print(enemy1)
    } else {
        print(enemy2)
    }
}
