(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' / Davide Gardenal / Christian Curtolo / Leonardo Tamai @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System.Collections

///Maze object containg the maze rapresentation and methods to create and solve the maze
type maze (w:int, h:int) as this =
    let mutable m = Array.create h (BitArray(w)) 
    let mutable solution:(int*int*bool) list = []
    let mutable s = new Generic.Stack<(int*int)> ();

    do this.initMaze ()

    ///Initialize and create the maze
    member public __.generate =
        this.initMaze ()    
        this.createMaze (w-2,h-2) //Difficoltà aumentata con questo trick
        s.Push((1,1))
        ()
    ///Maze Constructor
    member private __.initMaze () =
        for i in 0 .. h-1 do
            m.[i] <- new BitArray (w)
        ()
    ///Provides a random cell from a given list
    member private __.getRand (q) = 
        match q with
        | [] -> (-1,-1)
        | _ -> q.[rnd_int 0 (q.Length-1)]

    ///From a given cell returns a random valid nearby cell
    member private __.getRandomCell (r:int,c:int) =
        let mutable q = []
        if (r <> 1 && not(m.[r-2].Item c)) then q <- (r-2,c)::q
        if (r <> (h-2) && not(m.[r+2].Item c)) then q <-(r+2,c)::q
        if (c <> 1 && not(m.[r].Item (c-2))) then q <-(r,c-2)::q
        if (c <> (w-2) && not(m.[r].Item (c+2))) then q <-(r,c+2)::q
        this.getRand q

    ///Create the maze by getting an int row and an int column. if x,y = -1 then it works like a void method
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
    ///Returns the maze (bitArray)
    member public __.getMaze () = m
    ///Returns the weight of the maze
    member public __.getW() = w
    ///Returns the height of the maze
    member public __.getH() = h
    ///Returns true if the given position (r,c) if is inside the maze dimensions
    member public __.isValid (r,c) = if r<0 || c<0 || r>h-1 || c>w-1 then false
                                     else (m.[r].Item c)

    ///Returns true if the element is not in the given list
    member private __.not_in e l=
        match l with
            |[]->true
            |x::xs-> let (c,r,_) = x
                     if (c,r)=e then false
                     else __.not_in e xs

    ///Provides a list composed by the different position available
    member private __.av (x,y) (pos:(int*int*bool) list) =
        let mutable l:(int*int) list= []
        if __.isValid (y,x+1) && __.not_in (x+1,y) pos then l <- l@[(x+1,y)]//right
        if __.isValid (y+1,x) && __.not_in (x,y+1) pos then l <- l@[(x,y+1)]//down
        if __.isValid (y-1,x) && __.not_in (x,y-1) pos then l <- l@[(x,y-1)]//up
        if __.isValid (y,x-1) && __.not_in (x-1,y) pos then l <- l@[(x-1,y)]//left
        l
    
    ///Adds a list of cells to the internal stack.
    ///Used by oneStepSolver
    member private  __.toStack (l:(int*int) list)=
        match l with 
            |[] -> ()
            |[x] -> s.Push x
            |x::xs -> s.Push x
                      this.toStack xs

    
    ///Auxiliary method used by insertIntoSolution
    member private this.insertIntoSolutionAux (x:int,y:int) (sol:(int*int*bool) list)= 
        match sol with
        | [] -> [(x,y,false)]
        | h::tail -> let (xh,yh,_) = h
                     if xh = x && yh = y then
                        (x,y,false) :: tail
                     else
                        h::(this.insertIntoSolutionAux (x,y) tail)

    ///Utitlity method used to insert a cell (x,y) in the solution list
    member private this.insertIntoSolution (x:int,y:int) = 
        solution <- this.insertIntoSolutionAux (x,y) solution
        ()
    
    ///Method calld to solve the maze one step at the time
    member public __.oneStepSolve ()=
        if s.Count = 0 then false
        else
            let (x,y) = s.Pop ()
            
            if x = (w-2) && y = (h-2) then true     
            else
                let cord = this.av(x,y) solution
                if cord = [] then
                    this.insertIntoSolution(x,y)
                else
                    s.Push (x,y)
                    this.toStack(cord)
                    solution <- solution@[(x,y,true)]
                false
    ///Getter used to get the list contaning the solution (x,y,valid).
    ///Valid is a bool indicating if the cell (x,y) is the correct path or not
    member public __.getSolution () = solution 

    ///Returns true if the given position (x,y) is in the given list of positions
    member private __.isIn (x:int,y:int) (sol:(int*int*bool) list) = 
        match sol with
        |[] -> (-1,-1,false)
        |(hx,hy,c)::tail -> if hx = x && hy = y then (hx,hy,c)
                            else this.isIn (x,y) tail 
    ///Returns true if the cell (x,y) is in the solution
    member public __.isInSolution (x:int,y:int) = this.isIn (x,y) solution
        