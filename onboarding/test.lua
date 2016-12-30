-- CodinGame planet is being attacked by slimy insectoid aliens.
-- <---
-- Hint:To protect the planet, you can implement the pseudo-code provided in the statement, below the player.


-- game loop
while true do
    enemy1 = io.read() -- name of enemy 1
    dist1 = tonumber(io.read()) -- distance to enemy 1
    enemy2 = io.read() -- name of enemy 2
    dist2 = tonumber(io.read()) -- distance to enemy 2
    
    -- Write an action using print()
    -- To debug: io.stderr:write("Debug message\n")
    

    -- You have to output a correct ship name to shoot ("Buzz", enemy1, enemy2, ...)
    if dist1 < dist2 then
        print(enemy1)
    else
        print(enemy2)
    end
end