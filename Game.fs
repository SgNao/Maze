(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Menu.fs: menu
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)
module LabProg2019.Game

open System
open Engine
open Gfx
open Maze

[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

[< NoEquality; NoComparison >]
type state2 = {
    sprites : (sprite)[]
    player : sprite
}

//H e W devono essere uguali e dispari
let W = 35
let H = 35
                                        
let GameModeOne (w, h, s_w, s_h) = 
    let engine = new engine(W * 2, H)
    engine.show_fps <- false 
    
    let maze = new maze(w, h)
    maze.generateMaze(s_w, s_h)
    let maze_path = engine.create_and_register_sprite (image.duplicate(maze.drawMazeWall()), 0, 0, 0) 

    let player = engine.create_and_register_sprite ((image.rectangle (2, 1, pixel.filled Color.Red)), 2, 1, 2)
      
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (inf : info) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -2., 0.
            | 'd' -> 2., 0.
            | _   -> 0., 0.

        /// if the player is at the end point it shows the message
        if ((int(st.player.x + dx), int(st.player.y + dy)) = ((w * 2) - 2, (h - 2))) then
            ignore <| engine.create_and_register_sprite (image.rectangle(w * 2, h, pixel.filled Color.DarkGreen, pixel.filled Color.Black), 0, 0, 3)
            let maze_win = engine.create_and_register_sprite (image.rectangle(w * 2, h, pixel.filled Color.DarkGreen, pixel.filled Color.Black), 0, 0, 4)
            maze_win.draw_text("YOU WIN!\nPress q to return to the menu", w / 2, (h / 2), Color.White)

        ///blocks movements if there is a wall in the position I want to go to
        if(maze_path.get(int(st.player.x + dx), int(st.player.y + dy)) <> pixel.wall) then 
            st.player.move_by(dx, dy)
        else 
            st.player.move_by(0., 0.)
                      
        st, key.KeyChar = 'q'
      
    let st0 = { 
        player = player
    }
    engine.loop_on_key my_update st0


let GameModeTwo (w, h, s_w, s_h) =  
    let engine = new engine(W * 2, H)
    engine.show_fps <- false
    
    let maze = new maze(w, h)
    maze.generateMaze(s_w, s_h)
    let maze_path = engine.create_and_register_sprite (image.duplicate(maze.drawMazeWall()), 0, 0, 0)
    ignore <| engine.create_and_register_sprite (image.duplicate(maze.drawMazeSolution()), 0, 0, 1)

    let player = engine.create_and_register_sprite ((image.rectangle (2, 1, pixel.filled Color.Red)), 2, 1, 2)
      
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (inf : info) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -2., 0.
            | 'd' -> 2., 0.
            | _   -> 0., 0.

        /// if the player is at the end point it shows the message
        if ((int(st.player.x + dx), int(st.player.y + dy)) = ((w * 2) - 2, (h - 2))) then
            ignore <| engine.create_and_register_sprite (image.rectangle(w * 2, h, pixel.filled Color.DarkGreen, pixel.filled Color.Black), 0, 0, 3)
            let maze_win = engine.create_and_register_sprite (image.rectangle(w * 2, h, pixel.filled Color.DarkGreen, pixel.filled Color.Black), 0, 0, 4)
            maze_win.draw_text("YOU WIN!\nPress q to return to the menu", w / 2, (h / 2), Color.White)

        ///blocks movements if there is a wall in the position I want to go to
        if(maze_path.get(int(st.player.x + dx), int(st.player.y + dy)) <> pixel.wall) then 
            st.player.move_by(dx, dy)
        else 
            st.player.move_by(0., 0.)
               
        st, key.KeyChar = 'q'
      
    let st0 = {
        player = player
    }
    engine.loop_on_key my_update st0

let GameModeThree () =  
    let engine = new engine(W * 2, H + 5)
    engine.show_fps <- false

    let mutable controllo = true
    let mutable locale = double(0.0)
    let mutable cellVisited = []
    let mutable cellNotWall = []
    let mutable n_coin = 0
    let mutable cellGhost = (1, H-6) :: []
    
    let maze = new maze(W, H - 4)
    maze.generateMaze(W - 2, H - 6)
    maze.generateCoin()

    let maze_path = engine.create_and_register_sprite (image.duplicate(maze.drawMazeWall()), 0, 0, 0)
    let maze_coin = engine.create_and_register_sprite (image.duplicate(maze.drawMazeCoin()), 0, 0, 1)
    ignore <| engine.create_and_register_sprite ((image.rectangle (W, H, pixel.filled Color.Black)), 2, 41, 1)
    let player = engine.create_and_register_sprite ((image.rectangle (2, 1, pixel.filled Color.Red)), 2, 1, 2)
    
    for x in 10..W-2 do
        for y in 6..H - 6 do
            if (maze_path.get(x*2, y) <> pixel.wall) then 
                cellGhost <- (x*2, y) :: cellGhost
    Log.msg "Test %A" cellGhost

    ///creazione ghost
    let sprites = [| 
        for i in 0..4 do
            let x, y = cellGhost.Item(rnd_int 0 (cellGhost.Length - 1))
            cellGhost <- List.filter((<>) (x, y)) cellGhost

            let spr = engine.create_and_register_sprite (image.rectangle (2, 1, pixel.filled Color.Green), x, y, 3)
    
            yield spr
    |]

    let my_update (keyo : ConsoleKeyInfo option) (screen : wronly_raster) (inf : info) (st : state2) =
        let speed =
            match keyo with
            | None -> ()
            | Some key ->
                let dx, dy =
                    match key.KeyChar with 
                    | 'w' -> 0., -1.
                    | 's' -> 0., 1.
                    | 'a' -> -2., 0.
                    | 'd' -> 2., 0.
                    | _   -> 0., 0.
                
                /// if the player is at the end point it shows the message
                if ((int(st.player.x + dx), int(st.player.y + dy)) = ((W * 2) - 2, (H - 6))) then
                    let beack =  engine.create_and_register_sprite (image.rectangle(W * 2, H, pixel.filled Color.DarkGreen, pixel.filled Color.Black), 0, 0, 3)
                    let maze_win = engine.create_and_register_sprite (image.rectangle(W * 2, H, pixel.filled Color.DarkGreen, pixel.filled Color.Black), 0, 0, 4)
                    let p = sprintf "YOU WIN!\nPress q to return to the menu. \nN. coin: %d" n_coin
                    maze_win.draw_text(p, W / 2, (H / 2), Color.White)

                ///if 15 seconds have passed, the drill can be activated
                if (inf.timer - locale > double(15.0000) && (dx, dy) = (0., 0.)) then
                    locale <- inf.timer //salvo il valore all'attivazione
                    controllo <- false

                ///if 5 seconds have passed the drill is deactivated
                if (inf.timer > double(locale + 5.0) && controllo = false) then
                    controllo <-true
        
                let checkCoin (x : int, y : int, maze : image) =
                    maze.get(x, y) = pixel.coin             
                
                let checkMovements (x : int, y : int, maze : image) =
                    maze.get(x, y) <> pixel.wall

                ///if I step on a coin, I increase the counter and remove it
                let checkForCoin (x : int, y : int) = 
                    if List.contains (x, y) cellVisited then 
                        cellVisited <- (x, y) :: cellVisited
                    else
                        ignore <| engine.create_and_register_sprite ((image.rectangle (2, 1, pixel.filled Color.Black)), int(st.player.x + dx), int(st.player.y + dy), 2)
                        n_coin <- n_coin + 1   
                        cellVisited <- (x, y) :: cellVisited 

                ///if the drill is activated the controls are deactivated, I keep only those to keep it inside the frames, if it passes over a wall it transforms the wall into a path
                if(controllo = true) then
                    if (checkMovements(int(st.player.x + dx), int(st.player.y + dy), maze_path)) && (checkCoin(int(st.player.x + dx), int(st.player.y + dy), maze_coin)) then
                        checkForCoin(int(st.player.x + dx), int(st.player.y + dy))
                        st.player.move_by(dx, dy)
                    else if (checkMovements(int(st.player.x + dx), int(st.player.y + dy), maze_path)) || (List.contains (int(st.player.x + dx), int(st.player.y + dy)) cellNotWall) then
                            st.player.move_by(dx, dy)
                    else 
                        st.player.move_by(0., 0.)
                else
                    if (int(st.player.x + dx) > 0 && int(st.player.x + dx) < (W * 2) - 2 && int(st.player.y + dy) > 0 && int(st.player.y + dy) < H - 5) then 
                        if not(checkMovements(int(st.player.x + dx), int(st.player.y + dy), maze_path)) then
                            ignore <| engine.create_and_register_sprite ((image.rectangle (2, 1, pixel.path)), int(st.player.x + dx), int(st.player.y + dy), 2)
                            cellNotWall <- (int(st.player.x + dx), int(st.player.y + dy)) :: cellNotWall

                        if (checkCoin(int(st.player.x + dx), int(st.player.y + dy), maze_coin)) then
                            checkForCoin(int(st.player.x + dx), int(st.player.y + dy))
                        st.player.move_by(dx, dy)
                    else 
                        st.player.move_by(0., 0.)
   
        let checkMovements (x : int, y : int, maze : image) =
            maze.get(x, y) <> pixel.wall

        ///sprites movement
        let sprites = [|
            let d = [(0., -1.); (-2., 0.); (2., 0.); (0., 1.)]
            for spr in st.sprites do
                let dx, dy = d.Item(rnd_int 0 (d.Length - 1))
                if (checkMovements(int(spr.x + dx), int(spr.y + dy), maze_path)) then
                    spr.move_by (dx, dy)
                if(int(spr.x + dx), int(spr.y + dy)) = (int(st.player.x), int(st.player.y)) then 
                    let maze_lose = engine.create_and_register_sprite (image.rectangle(W * 2, H, pixel.filled Color.DarkRed, pixel.filled Color.Black), 0, 0, 4)
                    maze_lose.draw_text("YOU LOST!\nPress q to return to the menu", W / 2, (H / 2), Color.White)
                yield spr
            |]

        { sprites = sprites     // bind the new sprite array that we recalculate every frame
          player = st.player    // this does not change
          }, match keyo with None -> false | Some k -> k.KeyChar = 'q'
    
    let st0 = { 
        sprites = sprites
        player = player
        }
    // start engine loop
    engine.loop my_update st0
   
let level (mode : int) =
    let w = 21
    let h = 21

    let engine = new engine(W * 2, H)
    engine.show_fps <- false

    let player = engine.create_and_register_sprite ((image.rectangle (1, 1, pixel.filled Color.White)), 4, 3, 1)
    let instruction = engine.create_and_register_sprite ((image.rectangle (W, H, pixel.filled Color.Black)), 0, 0, 1)
    
    /// Show the instructions
    instruction.draw_text ("Select level", 4, 1, Color.Red)
    instruction.draw_text ("Easy", 6, 3, Color.White)
    instruction.draw_text ("Hard", 6, 5, Color.White)
      
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (inf : info) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -2.
            | 's' -> 0., 2.
            | _   -> 0., 0.

        ///check that the player is within a range
        if ((int(st.player.y + dy) < 3) || (int(st.player.y + dy) > 5)) then 
            st.player.move_by(0., 0.)
        else if (int(st.player.y + dy) = 3 && key.KeyChar = ' ') then
                st.player.move_by(dx, dy)
                if mode = 1 then
                    GameModeOne(w, h, 1, 1)
                else
                    GameModeTwo(w, h, 1, 1)                   
        else if (int(st.player.y + dy) = 5 && key.KeyChar = ' ') then
                st.player.move_by(dx, dy)
                if mode = 1 then
                    GameModeOne(H, W, w - 2, h - 2)
                else
                    GameModeTwo(H, W, w - 2, h - 2)
        else
            st.player.move_by(dx, dy)
            
        st, key.KeyChar = 'q'

    let st0 = { 
        player = player
    }
    engine.loop_on_key my_update st0

let main () =  
    let engine = new engine(W * 2, H)
    engine.show_fps <- false

    let player = engine.create_and_register_sprite ((image.rectangle (1, 1, pixel.filled Color.White)), 4, 3, 1)
    let instruction = engine.create_and_register_sprite ((image.rectangle (W, H, pixel.filled Color.Black)), 0, 0, 1)
    
    /// Show the instructions
    instruction.draw_text ("Welcome", 4, 1, Color.Red)
    instruction.draw_text ("Interactive mode", 6, 3, Color.White)
    instruction.draw_text ("Automatic mode", 6, 5, Color.White)
    instruction.draw_text ("Pacman mode", 6, 7, Color.White)
    instruction.draw_text ("How to play", 4, 9, Color.Red)
    instruction.draw_text ("Use wasd to move your player:\n press w: up\n press s: down\n press a: left\n press d: right", 6, 10, Color.White)
    instruction.draw_text ("Interactive mode:\n Use wasd to move your player", 6, 16, Color.White)
    instruction.draw_text ("Automatic mode:\n Use wasd to move your player and and run along the blue line", 6, 19, Color.White)
    instruction.draw_text ("Pacman mode:\n Use wasd to move your player collect the coins and\n Press the spacebar to\n activate drill mode", 6, 22, Color.White)
      
    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (inf : info) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -2.
            | 's' -> 0., 2.
            | _   -> 0., 0.

        if ((int(st.player.y + dy) < 3) || (int(st.player.y + dy) > 7)) then 
            st.player.move_by(0., 0.)
        else if (int(st.player.y + dy) = 3 && key.KeyChar = ' ') then
                st.player.move_by(dx, dy)
                level(1)
        else if (int(st.player.y + dy) = 5 && key.KeyChar = ' ') then
                st.player.move_by(dx, dy)
                level(2)
        else if (int(st.player.y + dy) = 7 && key.KeyChar = ' ') then
                st.player.move_by(dx, dy)
                GameModeThree()
        else
            st.player.move_by(dx, dy)
            
        st, key.KeyChar = 'q'

    let st0 = { 
        player = player
    }
    engine.loop_on_key my_update st0