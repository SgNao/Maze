(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open External
open Gfx
open System.Text

let rnd = System.Random()

type CharInfo with
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)

    static member path = pixel.filled Color.Black

    static member internal solution = pixel.filled Color.Blue

    static member internal coin = pixel.filled Color.DarkYellow

    member this.isWall = this = pixel.wall

// TASK 1: implement the maze type
type maze (w, h) =
    let mutable coins = (2, 1) :: []
    let mutable c = []

    member val wall = Array2D.init w h (fun x y -> not(x % 2 = 1 && y % 2 = 1)) //array2D che tiene conto di quali coordinate siano muri
    
    ///array2D inizializzato all'inizio tutto a false che mi tiene conto di quali celle io abbia visitato
    member val visited = Array2D.create w h false with get, set 

    member val solution = Array2D.create w h (1, 1) : (int * int)[ , ]

    member val stack = new System.Collections.Generic.Stack<int * int>()

    ///Metodo che date due coordinate verifica se sono muro
    member private this.isWall(x, y) = 
        this.wall.[x, y]

    ///Metodo che date due coordinate verifica se sono state visitate, restituisce true o false
    member private this.isVisited(x, y) =
        this.visited.[x, y]

    ///Metodo che date due coordinate verifica che siano all'interno del "range", restituisce true o false
    member private this.isLegalPoint(x, y) =
        x >= 0 && y >= 0 && x < w && y < h
    
    ///Metodo che date due coordinate restituisce una lista con le possibili coordinate vicine legali
    member private this.neighborsGenerate(x, y) =
        [(x + 2, y); (x - 2, y); (x, y + 2); (x, y - 2)] |> List.filter(this.isLegalPoint) |> List.sortBy (fun _ -> rnd.Next()) 

    ///Metodo che date due coordinate restituisce una lista con le possibili coordinate vicine legali
    member private this.neighborsSolution(x, y) =
        [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] |>List.filter (this.isLegalPoint) |> List.filter (not << this.isWall)

    ///Metodo che rimuove il muro tra due coordinate
    member private this.removeWall((x1, y1), (x2, y2)) =
        if (x1 + 2, y1) = (x2, y2) then
            this.wall.[x1 + 1, y1] <- false
        
        if (x1 - 2, y1) = (x2, y2) then
            this.wall.[x1 - 1, y1] <- false

        if (x1, y1 + 2) = (x2, y2) then
            this.wall.[x1, y1 + 1] <- false

        if (x1, y1 - 2) = (x2, y2) then
            this.wall.[x1, y1 - 1] <- false

    ///Metodo che genera il maze
    member this.generateMaze(s_w, s_h) =
        let rec generate(x, y) =
            this.stack.Push(x, y)
            this.visited.[x, y] <- true

            while this.stack.Count > 0 do
                let currentCell = this.stack.Pop()
                let neighbor = this.neighborsGenerate(currentCell)
                                
                match List.tryFind(not << this.isVisited) neighbor with
                | Some (n_x, n_y as vicini) -> this.visited.[n_x, n_y] <- true
                                               this.removeWall(currentCell, vicini) |> ignore
                                               this.stack.Push(currentCell)
                                               this.stack.Push(vicini)
                | None -> ()
                   
        this.wall.[w - 1, h - 2] <- false //apertura muro per punto finale
        this.stack.Clear()
        generate (s_w, s_h)

    ///Metodo che genera la soluzione
    member this.generateSolution() =
        let generate(x, y) =
            this.stack.Push(x, y)
            this.visited.[x, y] <- true

            while this.stack.Count > 0 do
                let currentCell = this.stack.Pop()
                let neighbor = this.neighborsSolution(currentCell)
                match List.tryFind(not << this.isVisited) neighbor with
                | Some (n_x, n_y as vicini) -> this.stack.Push(currentCell)
                                               this.stack.Push(vicini)
                                               this.visited.[n_x, n_y] <- true
                                               this.solution.[n_x, n_y] <- currentCell
                | None -> ()
        
        this.visited <- Array2D.init w h (fun _ _ -> false)
        this.stack.Clear()  
        generate (w - 2, h - 2)
    
    ///Metodo che trasmorma l'array2D in un image
    member this.drawMazeWall() = 
        image(w, h,[|                 
            for y in 0..h - 1 do
                for x in 0..w - 1 do                   
                    if this.wall.[x, y] then
                        yield CharInfo.wall    
                    else 
                        yield CharInfo.path
        |])
    
    ///Metodo che trasmorma l'array2D in un image
    member this.drawMazeSolution() =
        this.generateSolution()
        
        let mutable solution = (1, 1) :: []

        while not(List.head(solution) = ((w - 2), (h - 2))) do
            let x, y = List.head(solution)
            solution <- this.solution.[x, y] :: solution

        image(w, h, [|
        for y in 0..h - 1 do
            for x in 0.. w - 1 do
                if List.contains (x, y) solution then
                    yield CharInfo.solution
                else
                    yield CharInfo.empty
        |])

    ///Metodo per generare le monete
    member this.generateCoin() = 
        let mutable count = 0

        for y in 0..h - 1 do
            for x in 0.. w - 1 do
                if not(this.wall.[x, y]) then
                    coins <- (x, y) :: coins
                else
                    ()

        while count < 9 do
            let extract = coins.Item(rnd_int 0 (coins.Length - 1))
            c <- extract :: c
            coins <- List.filter((<>) extract) coins
            count <- count + 1

    ///Metodo che trasmorma la lista in un image
    member this.drawMazeCoin() =
        image(w, h, [|
        for y in 0..h - 1 do
            for x in 0.. w - 1 do
                if List.contains (x, y) c then
                    yield CharInfo.coin
                else
                    yield CharInfo.empty
        |])
        
