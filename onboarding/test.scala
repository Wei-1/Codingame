object Player extends App {

    // game loop
    while(true) {
        val enemy1 = readLine // name of enemy 1
        val dist1 = readInt // distance to enemy 1
        val enemy2 = readLine // name of enemy 2
        val dist2 = readInt // distance to enemy 2
        
        // Write an action using println
        
        // Enter the code here

        if (dist1 < dist2) {
            println(enemy1)
        } else {
            println(enemy2)
        }
    }
}