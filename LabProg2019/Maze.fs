(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System.Collections

//TODO documentare e risolutore automatico
type maze (w:int, h:int) as this =
    let mutable m = Array.create h (BitArray(w)) 
    let mutable solution:(int*int) list = []

    do this.initMaze ()

    
    member public __.generate =
        this.initMaze ()    
        this.createMaze (w-2,h-2) //difficolta aumentata con questo trick
        solution <- this.solve (1,1) []
        ()
    
    member private __.initMaze () =
        for i in 0 .. h-1 do
            m.[i] <- new BitArray (w)
        ()

    member private __.getRand (q) = 
        match q with
        | [] -> (-1,-1)
        | _ -> q.[rnd_int 0 (q.Length-1)]

    //da controllare le righe con le colonne (che non siano invertite)
    member private __.getRandomCell (r:int,c:int) =
        let mutable q = []
        if (r <> 1 && not(m.[r-2].Item c)) then q <- (r-2,c)::q
        if (r <> (h-2) && not(m.[r+2].Item c)) then q <-(r+2,c)::q
        if (c <> 1 && not(m.[r].Item (c-2))) then q <-(r,c-2)::q
        if (c <> (w-2) && not(m.[r].Item (c+2))) then q <-(r,c+2)::q
        this.getRand q
     
    member private __.createMaze (r:int,c:int) = 
        let row  = m.[r]
        row.Set (c,true);
        let (x,y) = this.getRandomCell (r,c)
       
        if x = -1 && y = -1 then ()
        else 
             if x = r then 
                if y = c-2 then m.[r].Set (c-1,true)
                else m.[r].Set (c+1,true)
             else if x = (r-2) then m.[r-1].Set (c,true)
             else m.[r+1].Set (c,true)

             this.createMaze (x,y)
             this.createMaze (r,c)
            
    member public __.getMaze () = m
    member public __.getW() = w
    member public __.getH() = h
    

    //member public __.isValid (r,c) = (m.[r].Item c)

    member public __.isValid (r,c) = if r<0 || c<0 || r>h-1 || c>w-1 then false
                                     else (m.[r].Item c)

    //true se l'elemento non è nella lista
    member private __.not_in e l=
        match l with
            |[]->true
            |x::xs->if x=e then false
                    else __.not_in e xs

    //posizioni valide
    member private __.av (x,y) pos=
        let mutable l:(int*int) list= []
        if __.isValid (y,x+1) && __.not_in (x+1,y) pos then l <- l@[(x+1,y)]//dx
        if __.isValid (y+1,x) && __.not_in (x,y+1) pos then l <- l@[(x,y+1)]//giù
        if __.isValid (y-1,x) && __.not_in (x,y-1) pos then l <- l@[(x,y-1)]//su
        if __.isValid (y,x-1) && __.not_in (x-1,y) pos then l <- l@[(x-1,y)]//sx
        l
    
    //(x,y)-> posizione attuale, sol-> passi compiuti, pos-> posizioni passate
    member private __.solve (x,y) (sol:(int*int) list)=
        //Log.msg "%A" (x,y)
        
        let mutable newSol = []
        if x = (w-2) && y = (h-2) then sol@[(x,y)] 
        else
            let coords = this.av (x,y) sol
            //Log.error "%A" coords
            if coords = [] then 
                []            
            else
                for i in 0 .. (coords.Length-1) do
                    let temp = this.solve coords.[i] (sol@[(x,y)])
                    if temp <> [] then newSol <- temp 
                newSol

    member public __.getSolution () = solution 

    member private __.isIn (x:int,y:int) (sol:(int*int) list) = 
        match sol with
        |[] -> false
        |(hx,hy)::tail -> if hx = x && hy = y then true
                          else this.isIn (x,y) tail 

    member public __.isInSolution (x:int,y:int) = this.isIn (x,y) solution
        