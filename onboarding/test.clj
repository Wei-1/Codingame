(ns Player
    (:gen-class))

; CodinGame planet is being attacked by slimy insectoid aliens.
; <---
; Hint:To protect the planet, you can implement the pseudo-code provided in the statement, below the player.

(defn -main [& args]
    (while true
        (let [enemy1 (read) dist1 (read) enemy2 (read) dist2 (read)]
            ; enemy1: name of enemy 1
            ; dist1: distance to enemy 1
            ; enemy2: name of enemy 2
            ; dist2: distance to enemy 2
        
            ; (binding [*out* *err*]
            ;   (println "Debug messages..."))
        
            ; You have to output a correct ship name to shoot ("Buzz", enemy1, enemy2, ...)
            (if ( < dist1 dist2 ) (println enemy1) (println enemy2))
        )
    )
)