module LabProg2019.GameController

open System
open External
open Gfx
open Engine
open Maze

let W = 51;
let pixelW = W*2
let H = 51;

type CharInfo with
    static member wall = pixel.create ('#', Color.White, Color.White)
    static member internal path = pixel.filled Color.Black
    member this.isWall = this = pixel.wall

type menuState = {
    mutable isVisible : bool
    items : string []
    coords : (int*int) []
    mutable selectedIndex : int
}

type mazeState = {
    mutable isVisible : bool //visible flag (needed for rendering)
    mutable s : sprite  //start point
    mutable e : sprite  //end point
    mutable m : maze    //maze obj
    mutable mSprites : sprite list //non so se servono effettivamente
}

type state = {
    engi : engine
    mutable menustate : menuState
    mutable mazestate : mazeState 
    mutable player : sprite
    mutable terminate : bool 
}

let print (m:maze,engine:engine) = engine.create_and_register_sprite (image.maze (m) false, 0,0,0)


let printMenu (screen:wronly_raster) (st:menuState) = 
    for i in 0 .. (st.items.Length-1) do
        let (x,y) = st.coords.[i]
        screen.draw_text(st.items.[i],x,y,Color.Black,Color.White)
            
let getMenuCoords (items:string[]) = 
    let mutable coords = [((pixelW/2)-(items.[0].Length/2),H/5)]
    let yincr = int((float(H) * (3./5.)) / float(items.Length-1))
    let ystart = H/5
    for i in 1 .. items.Length-1 do
        coords <- coords@[((pixelW/2)-(items.[i].Length/2),ystart+(i*yincr))]
    List.toArray coords

let getMenuItems () = [|"--Select Game mode--";"--Press m to return here--";"# Classic #";"# Automated #";"# Work in progress #";"# Exit #"|]

let getMazeEndPointSprite () = new sprite (image.rectangle (2,1, pixel.create ('*',Color.Blue,Color.Blue)), ((W-2)*2),(H-2), 1)
let getMazeStartPointSprite ()= new sprite (image.rectangle (2,1, pixel.create ('*',Color.Red,Color.Red)), 2, 1, 1) 
let spawnPlayer (engi:engine) = engi.create_and_register_sprite (image.rectangle (2,1, pixel.create ('#',Color.Green,Color.Green)), 2, 1, 2) //creare il player 
let initState (engi:engine) = //status initializer
    {
        engi = engi
        menustate = {
            isVisible = true
            items = getMenuItems () 
            coords = getMenuCoords (getMenuItems ())
            selectedIndex = -1
        }
        mazestate = {
            isVisible = false
            s = getMazeStartPointSprite ()
            e = getMazeEndPointSprite ()
            m = new maze(W,H)
            mSprites = []
        }
        player = spawnPlayer (engi)
        terminate = false
    }

let returnToMenu (st:state) =
    st.mazestate.isVisible <- false
    st.menustate.isVisible <- true
    st.engi.setSprites [] //delete all the sprites
    st.player <- spawnPlayer st.engi //respawn player
    st

let createAndPrintMaze (st:state) = 
    st.mazestate.m.generate
    st.mazestate.isVisible <- true
    st.menustate.isVisible <- false
    st.mazestate.mSprites <- print(st.mazestate.m, st.engi)::[]
    st.engi.register_sprite st.mazestate.e
    st.engi.register_sprite st.mazestate.s
    st.player.x <- 2.
    st.player.y <- 1.
    st

let playerMovmentHandler (key : ConsoleKeyInfo) =
        match key.KeyChar with 
        | 'w' -> 0., -1.
        | 's' -> 0., 1.
        | 'a' -> -2., 0.
        | 'd' -> 2., 0.
        | _   -> 0., 0.


let isPlayerOver (s:state) = //returns the index of the selected menu item
    let mutable selectedIndex = -1
    for i in 2 .. (s.menustate.items.Length-1) do           //ATTENZIONE AL INDICE DI PARTENZA
        let (x,y) = s.menustate.coords.[i]
        let width = s.menustate.items.[i].Length
        if int(s.player.x)  >= x && int(s.player.x) < (x+width) && int(s.player.y) = y then selectedIndex <- i
    selectedIndex


let menuHandler (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) (x:float,y:float)= 
    let mutable newState = st
    ignore <| newState.player.move_by (x, y) //è necessario perche il movimento in questo caso è incondizionato mentre quando il labirinto è visibile non lo è
    if key.KeyChar = 'e' then   //se l'utente preme 'e' bisogna abilitare la modalita visualizzata 
        if newState.menustate.selectedIndex = 2 then  //Classic //ATTENZIONE ALL'INDICE
            newState <- createAndPrintMaze newState
        //TODO add the other game modes here
        if newState.menustate.selectedIndex = (newState.menustate.items.Length-1) then 
            newState.terminate <- true
            
    newState.menustate.selectedIndex <- isPlayerOver newState //return the selected index of the menu
    printMenu screen newState.menustate     //prints the menu
    newState //ritorna lo stato modificato

let printWinMessage (screen : wronly_raster) = 
    let winMessage = "YOU WON!"
    screen.draw_text(winMessage , (screen.width/2)-(winMessage.Length/2),screen.height/2,Color.Black,Color.White)
    let retToMen = "Press a key to return to the menu"
    screen.draw_text(retToMen , (screen.width/2)-(retToMen.Length/2),screen.height/2 + 2,Color.Black,Color.White)

let mazeHandler (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) (x:float,y:float) = 
    let mutable newState = st
    let px = int(newState.player.x/2. + x/2.) //calcolo della posizione effettiva del player nella matrice del maze
    let py = int(newState.player.y + y)
    if newState.mazestate.m.isValid (py,px) then
        ignore <| newState.player.move_by (x, y)
        if newState.mazestate.e.x = newState.player.x && newState.mazestate.e.y = newState.player.y then
            newState.engi.setSprites ([])
            printWinMessage screen
            newState <- returnToMenu newState
    newState

let update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) = 
    let mutable newState = st               //new state to use
  
    let (x,y) = playerMovmentHandler (key)

    if key.KeyChar = 'm' then newState <- returnToMenu newState
    
    if newState.menustate.isVisible then 
        newState <- menuHandler key screen newState (x,y) //la funzione menuHandler ritorna lo stato modificato

    if newState.mazestate.isVisible then
        newState <- mazeHandler key screen newState (x,y) //la funzione mazeHandler ritorna lo stato modificato

    newState, st.terminate

let main () = 
    let engi = new engine (W*2,H)
    engi.show_fps <- false
    engi.loop_on_key update (initState (engi)) 