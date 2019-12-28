module LabProg2019.GameController

open System
open External
open Gfx
open Engine
open Maze
open System.Diagnostics

//Game Window size
let W = 61;
let H = 61;

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
    mutable isVisible : bool        //visible flag (needed for rendering)
    mutable s : sprite              //start point
    mutable e : sprite              //end point
    mutable m : maze                //maze obj
    mutable offx : int              //offset of the starting point of the maze
    mutable offy : int
    mutable mazeSprite : sprite
}

type state = {
    engi : engine
    mutable menustate : menuState
    mutable menudiffstate : menuState
    mutable mazestate : mazeState 
    mutable player : sprite
    mutable time : TimeSpan
    mutable timer : Stopwatch
    mutable changePlayer : bool     //flag used in order to wait in pvp when p1 won before p2 starts
    mutable terminate : bool 
}

///returns a pair rapresenting the maze dimensions given the index. Index is from 0 to 3 (0 -> easy / 3 -> master)
let getDiff (indx:int) = 
    let d = [|(11,11);(41,41);(51,51);(61,61)|]
    d.[indx-1]

///it just prints the maze or the solution and returns the sprite
let onlyPrintMaze (st:state) (pSol:bool) (zIndex:int) = st.engi.create_and_register_sprite (image.maze (st.mazestate.m) pSol, st.mazestate.offx,st.mazestate.offy,zIndex)

///it initialize and print the maze and the other sprites needed
let printMaze (st:state) = 
    st.mazestate.offx <- W - st.mazestate.m.getW () 
    st.mazestate.offy <- (H - st.mazestate.m.getH ())/2 
    
    st.mazestate.mazeSprite <- onlyPrintMaze st false 1

    st.mazestate.e.x <- float(2*(st.mazestate.m.getW()-2) + st.mazestate.offx)
    st.mazestate.e.y <- float(st.mazestate.m.getW()-2 + st.mazestate.offy)
    st.mazestate.s.x <- float(st.mazestate.offx) + 2. 
    st.mazestate.s.y <- float(st.mazestate.offy) + 1.
    st.engi.register_sprite st.mazestate.e
    st.engi.register_sprite st.mazestate.s
    st.player.x <- 2. + float(st.mazestate.offx)
    st.player.y <- 1. + float(st.mazestate.offy)
    st

let printMenu (screen:wronly_raster) (ms:menuState) = 
    for i in 0 .. (ms.items.Length-1) do
        let (x,y) = ms.coords.[i]
        screen.draw_text(ms.items.[i],x,y,Color.Black,Color.White)
  

let getMenuCoords (items:string[]) = 
    let pixelW = W*2
    let mutable coords = []
    let yincr = int((float(H) * (3./5.)) / float(items.Length-1))
    let ystart = H/5
    for i in 0 .. items.Length-1 do
        coords <- coords@[((pixelW/2)-(items.[i].Length/2),ystart+(i*yincr))]
    List.toArray coords

//TODO si potrebbe separare le voci del menu non selezionabili e quelle selezionabili 
let getMenuItems () = [|"--Select Game mode--";"--Press m to return here--";"# Classic #";"# Automated #";"# PvP #";"# Exit #"|]
let getMenuDiffItems () = [|"--Select a difficulty level--";"# Easy #";"# Normal #";"# Hard #";"# Master #"|]

let getMazeEndPointSprite () = new sprite (image.rectangle (2,1, pixel.create ('*',Color.Blue,Color.Blue)), (W-2)*2,H-2, 2)
let getMazeStartPointSprite ()= new sprite (image.rectangle (2,1, pixel.create ('*',Color.DarkYellow,Color.DarkYellow)), 2, 1, 2)

let spawnPlayer (engi:engine) = engi.create_and_register_sprite (image.rectangle (2,1, pixel.create ('#',Color.Green,Color.Green)), 2, 1, 3) //creare il player 
let initState (engi:engine) =                                                                   //status initializer
    {
        engi = engi
        menustate = {
            isVisible = true
            items = getMenuItems () 
            coords = getMenuCoords (getMenuItems ())
            selectedIndex = -1
        }
        menudiffstate = {
            isVisible = false
            items = getMenuDiffItems ()
            coords = getMenuCoords (getMenuDiffItems ())
            selectedIndex = -1 //no selected difficluty
        }
        mazestate = {
            isVisible = false
            s = getMazeStartPointSprite ()
            e = getMazeEndPointSprite ()
            m = new maze (0,0)
            offx = 0 //lo 0 non è realmente utilizato (solo di init)
            offy = 0
            mazeSprite = new sprite(image.rectangle(0,0,pixel.empty),0,0,0) //value not used
        }
        player = spawnPlayer (engi)
        time = new TimeSpan (int64(0))
        timer = null
        changePlayer = false
        terminate = false
    }

let returnToMenu (st:state) =
    st.mazestate.isVisible <- false
    st.menudiffstate.isVisible <- false
    st.menustate.isVisible <- true
    st.menustate.selectedIndex <- -1
    st.menudiffstate.selectedIndex <- -1
    st.engi.setSprites []                                                                       //delete all the sprites
    st.player <- spawnPlayer st.engi                                                            //respawn player
    st.changePlayer <- false
    st.time <- new TimeSpan (int64(0))
    st.timer <- null
    st

let createAndPrintMaze (st:state) = 
    st.mazestate.m <- new maze (getDiff (st.menudiffstate.selectedIndex))
    st.mazestate.m.generate
    st.mazestate.isVisible <- true
    st.menudiffstate.isVisible <- false
    printMaze(st)

let playerMovmentHandler (key : ConsoleKeyInfo) =
        match key.KeyChar with 
        | 'w' -> 0., -1.
        | 's' -> 0., 1.
        | 'a' -> -2., 0.
        | 'd' -> 2., 0.
        | _   -> 0., 0.

let isPlayerOver (ms:menuState) (p:sprite) (sIndx:int) =                                                                    //returns the index of the selected menu item
    let mutable selectedIndex = -1
    for i in sIndx .. (ms.items.Length-1) do                                               
        let (x,y) = ms.coords.[i]
        let width = ms.items.[i].Length
        if int(p.x)  >= x && int(p.x) < (x+width) && int(p.y) = y then selectedIndex <- i
    selectedIndex

let movePlayerMenu (dx:float,dy:float) (st: state)=
    let mutable xf = dx
    let mutable yf= dy
    //controlla se sfora di x
    if (st.player.x + dx)< 0. then xf <- float(W*2)
    if (st.player.x + dx)> float(W*2) then xf <- (-float(W*2))
    
    //controlla se sfora di y
    if (st.player.y + dy)< 0. then yf <- float(H)
    if (st.player.y + dy)> float(H) then yf <- (-float(H))
    
    st.player.move_by (xf, yf)

let diffChoserHandler (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) (x:float,y:float) = 
    let mutable newState = st
    
    movePlayerMenu (x, y) newState

    if key.KeyChar = 'e' && newState.menudiffstate.selectedIndex<>(-1) then //nel caso in cui il player è sopra una voce selezionabile del menu crea il maze
            newState <- createAndPrintMaze newState
    else    
        newState.menudiffstate.selectedIndex <- isPlayerOver newState.menudiffstate newState.player 1
        printMenu screen newState.menudiffstate
    newState

let menuHandler (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) (x:float,y:float) = 
    let mutable newState = st

    movePlayerMenu (x, y) newState                                                              //è necessario perche il movimento in questo caso è incondizionato mentre quando il labirinto è visibile non lo è
   
    if key.KeyChar = 'e' && newState.menustate.selectedIndex<>(-1) then                         //se l'utente preme 'e' bisogna abilitare la modalita visualizzata solo se lo sprite del player è sopra una voce del menu selezionabile
        if newState.menustate.selectedIndex = (newState.menustate.items.Length-1) then 
            newState.terminate <- true
        else
            newState.menustate.isVisible <- false 
            newState.menudiffstate.isVisible <- true
    else                                                                                        //altrimenti 
        newState.menustate.selectedIndex <- isPlayerOver newState.menustate newState.player 2   //return the selected index of the menu  //2 è l'indice del primo elemento selezionabile
        printMenu screen newState.menustate                                                     //prints the menu
    newState//ritorna lo stato modificato

let printWinMessage (screen : wronly_raster) = 
    let winMessage = "YOU WON!"
    screen.draw_text(winMessage , (screen.width/2)-(winMessage.Length/2),screen.height/2,Color.Black,Color.White)
    let retToMen = "Press a key to return to the menu"
    screen.draw_text(retToMen , (screen.width/2)-(retToMen.Length/2),screen.height/2 + 2,Color.Black,Color.White)

let printPvPWinM (screen : wronly_raster) (t1:TimeSpan) (t2:TimeSpan)= 
    let mutable winMessage = "Player 1 WON!"
    if t2<t1 then
        winMessage <- "Player 2 WON!"                
    screen.draw_text(winMessage , (screen.width/2)-(winMessage.Length/2),screen.height/2,Color.Black,Color.White)

    let p1 = "Player 1 time : " + string(t1.Minutes) + " minutes and " + string(t1.Seconds) + " seconds"
    screen.draw_text(p1 , (screen.width/2)-(p1.Length/2),screen.height/2 + 2,Color.Black,Color.White)

    let p2 = "Player 2 time : " + string(t2.Minutes) + " minutes and " + string(t2.Seconds) + " seconds"
    screen.draw_text(p2 , (screen.width/2)-(p2.Length/2),screen.height/2 + 4,Color.Black,Color.White)

    let retToMen = "Press a key to return to the menu"
    screen.draw_text(retToMen , (screen.width/2)-(retToMen.Length/2),screen.height/2 + 6,Color.Black,Color.White)

let onMazePlayerHandler (screen : wronly_raster) (st : state) (x:float,y:float) = //returns if u won and the changed state
    let mutable newState = st
    let mutable win = false
    let px = (int(newState.player.x) - newState.mazestate.offx)/2 + int(x)/2                          //calcolo della posizione effettiva del player nella matrice del maze
    let py = int(newState.player.y + y) - newState.mazestate.offy                                     //tenendo contro dell'eventuale offset dall'angolo
    if newState.mazestate.m.isValid (py,px) then
        ignore <| newState.player.move_by (x, y)
        if newState.mazestate.e.x = newState.player.x && newState.mazestate.e.y = newState.player.y then
            win <- true
    (newState,win)

let mazeHandler (screen : wronly_raster) (st : state) (x:float,y:float) = 
    let mutable newState = st
    if newState.menustate.selectedIndex = 2 then                                        //Classic
        let (state,win) = onMazePlayerHandler screen st (x,y)
        newState <- state
        if win then
            printWinMessage screen
            newState <- returnToMenu newState


    else if newState.menustate.selectedIndex = 3 then                                   //Automated
        newState.player.z <- -1 //hide the player        
        //update the solution
        let win = newState.mazestate.m.oneStepSolve () //returns if u win or not
        //delete the previous solution sprite
        newState.engi.deleteSprite (newState.mazestate.mazeSprite)       
        //and print the updated soloution
        newState.mazestate.mazeSprite <- onlyPrintMaze newState true 1
        if win then
            newState.player.z <- 3 //show the player
            printWinMessage screen
            newState <- returnToMenu newState


    else if newState.menustate.selectedIndex = 4 then                                   //PvP
        if newState.changePlayer then   //controllo che indica se siamo nella schermata di cambio player
            newState.changePlayer <- false  //setta a false il cambio del player 
            newState <- createAndPrintMaze newState //crea e printa il maze nuovo
            newState.engi.register_sprite newState.player //spawna il player
        else        //altrimenti se siamo in una situazione normale
            //start timer
            if newState.timer = null then
                newState.timer <- Stopwatch.StartNew()
            let (state,win) = onMazePlayerHandler screen st (x,y)   //gestisci il movimento del player
            newState <- state   //aggiorna lo stato
            if win then //se abbiamo winto
                newState.timer.Stop () //ferma il tempo
                newState.engi.setSprites [] //elimina il maze precendente
                if newState.time.Ticks = int64(0) then  //se il tempo è a 0 (valore di default)
                    newState.time <- newState.timer.Elapsed //setta il nuovo tempo del player 1
                    let holdMessage = "Change player"
                    screen.draw_text(holdMessage , (screen.width/2)-(holdMessage.Length/2),screen.height/2,Color.Black,Color.White) //printa il messaggio di cambio player
                    newState.changePlayer <- true   //setta il flag di cambio player a true
                else //altrimenti
                    printPvPWinM screen newState.time newState.timer.Elapsed    //printa il messaggio finale (con i tempi e il vincitore)
                    newState.time <- new TimeSpan (int64(0)) //reset del tempo
                    newState <- returnToMenu newState   //ritorna al menu
                newState.timer <- null //elimina il timer usato

    newState

let update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) = 
    let mutable newState = st                                                                   //new state to use
  
    let (x,y) = playerMovmentHandler (key)

    if key.KeyChar = 'm' then newState <- returnToMenu newState

    if newState.menustate.isVisible then 
        newState <- menuHandler key screen newState (x,y)                                       //la funzione menuHandler ritorna lo stato modificato

    if newState.menudiffstate.isVisible then 
        newState <- diffChoserHandler key screen newState (x,y)

    if newState.mazestate.isVisible then
        newState <- mazeHandler screen newState (x,y)                                       //la funzione mazeHandler ritorna lo stato modificato

    newState, st.terminate

let main () = 
    let engi = new engine (W*2,H)
    engi.show_fps <- false
    engi.loop_on_key update (initState (engi)) 