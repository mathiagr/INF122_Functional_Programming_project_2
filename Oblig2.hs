--Mathias Grønstad
--Answer: b 3 3 s 2 3 5 0 2 1 0 1 2 2 1 2 2

--More interesting answer:
--s 2 3 b 3 3 15 0 11 0 13 0 14 1 2 2 14 4 11 8 13 11 11 11 12 13 2 13 13 14 3 1 2 1 4 1 5 2 4 3 5 3 6 4 9 12 5 12 6 12 8 12 9 12 8 14 6 13 9 7 9 11 8 11 9 4 8 3 9 7 9 7 3 

import Data.Char
import Data.List (sort)
import Data.List
import System.Random

--Clear screen function
cls::IO()
cls = putStr "\ESC[2J"

--Creating a nxn "Matrix" of '.' 
square :: Int -> [[Char]]
square n = [['.' | a <- [0..n-1]] | b <- [0..n-1]]

--New Types
type Matrix = [[Char]] --the Matrix is the actual array of symbols which are printed on the screen
type Pos = (Int, Int) --positions are used as coordinates on the Board type
type Board = [Pos] --where we store the living cell coordinates

--Funciton for showing a matrix on the screen
visMatrise :: Matrix -> IO ()
visMatrise [[]] = putStrLn("")
visMatrise [] = putStrLn("")
visMatrise m = do                             
                let dim = length m
                putStr("")
                --Showing the indices of the columns as the first row:
                let upperRow k = do let a = dim - k+1
                                    if (a<10) then putStr("   " ++ show (a))
                                    else putStr("  " ++ show (a))
                                    if (k>1) then upperRow (k-1) else putStrLn("")
                upperRow (length m)
                --Showing the rows of the matrix including row indices:                               
                let showRows (x:xs) = do
                                   let a = dim - (length xs)
                                   putStrLn("")
                                   if a <10 then putStrLn (show a ++ "  " ++ id (intersperse ' ' (intersperse ' ' x)))
                                   else putStrLn (show a ++ " " ++ id (intersperse ' ' (intersperse ' ' x)))
                                   if (length xs>0) then showRows xs else putStrLn("")
                showRows m

--Function for updating an element in a matrix                
updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix matrix new (x,y) =
  take x matrix ++ -- taking the first x rows and adding...
  [take y (matrix !! x) ++ -- the first y elements of the x'th row, and...
  [new] ++ --appending the new element and...
  drop (y + 1) (matrix !! x)] ++ --appending the the rest of the elements in the x'th row and ...
  drop (x + 1) matrix -- appending the rest of the rows in the matrix.

--Checking if a position is alive (exists as a coordinate in the "Board")
isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

--Checking if empty (opposite of alive)
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

--Taking one position and giving a list of its neighboring positions
--Corners have 3 neighbors, wall cells have 5, and other cells have 8
neighbs :: Int -> Pos -> [Pos]
neighbs dim (x,y) = if(y==0 && x==0) then 
                        [(x,y+1), (x+1, y+1), (x+1, y)]
                    else if(y==0 && x==dim) then
                        [(x-1,y), (x-1,y+1), (x,y+1)]
                    else if(y==dim && x==0) then
                        [(x, y-1), (x+1, y-1), (x+1, y)]  
                    else if(y==dim && x==dim) then
                        [(x-1,y-1), (x-1,y), (x, y-1)] 
                    else if (x==0) then 
                        [(x,y+1), (x, y-1), (x+1, y+1), (x+1, y-1), (x+1, y)] 
                    else if(y==0) then 
                        [(x-1,y), (x-1,y+1), (x,y+1), (x+1, y+1),(x+1, y)]
                    else if(y==dim) then  
                        [(x-1,y-1), (x-1,y), (x, y-1), (x+1, y-1), (x+1, y)]  
                    else if(x==dim) then
                        [(x-1,y-1), (x-1,y), (x-1,y+1), (x,y+1), (x, y-1)]
                    else if (x>0 && x<dim && y>0 && y<dim) then
                        [(x-1,y-1), (x-1,y), (x-1,y+1), (x,y+1), (x, y-1), (x+1, y+1),(x+1, y-1), (x+1, y)]
                    else []

--Checking which of the neighboring cells of a cell is alive
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . (neighbs (length b-1))

--Checking which cells will survive the next generation (according to the rules)
survivors :: Board -> Int -> Int -> [Pos]
survivors b s1 s2 = [p | p <- b, elem (liveneighbs b p) [s1..s2]]

----Checking which cells will become alive the next generation (according to the rules)
births :: Board -> Int -> Int -> Int -> [Pos]
--births b = [(x,y) | x <- [1..length b], y <- [1..length b], isEmpty b (x, y), liveneighbs b (x, y) == 3] --considers every cell, below is more effecive
births b b1 b2 dim  = [p | p <- rmdups(concat(map (neighbs (length b-1)) b)),fst  p< dim, snd p < dim, isEmpty b p, elem (liveneighbs b p) [b1..b2]] 

--Function for removing duclicates
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

--Function for creating the next generation (creating a new Board of cells which are alive)
nextgen :: Board -> Int -> Int -> Int -> Int -> Int -> Board
nextgen b s1 s2 b1 b2 dim = survivors b s1 s2 ++ births b b1 b2 dim

--Reading a "matrix" (containing characters) into a "board"
readMatrixToBoard :: Matrix -> Board
readMatrixToBoard m = [(x,y) | x <- [0..length m-1], y <- [0..length m-1], m!!x!!y /= '.']

--Creating a Matrix from a Board
readBoardToMatrix :: Matrix -> Board -> Matrix
readBoardToMatrix m [] = m
readBoardToMatrix m (b:bs) = readBoardToMatrix (updateMatrix m 'X' b) bs

--Function for waiting to create delay for displaying generations
wait :: Int -> IO()
wait n = sequence_ [return() | zZz <- [0..n]]

--Creaing an game with an initial/empty nxn Matrix
c :: Int -> IO()
c n = game (square n) [2, 3, 3, 3] [] --setting initial rules s 2 3 b 3 3

--Create history of boards
newboards :: Board -> [Board] -> [Board]
newboards newboard boards = newboard : boards --reverse (newboard : (reverse boards))

--function for reading a file and starting new game
loadgame :: String -> IO()
loadgame f = do
    let (s:s1:s2:b:b1:b2:n:board) = words f --splitting input string to relevant parameters
    --let rules = [read s1::Int, read s2::Int, read b1::Int, read b2::Int] --assigning rules
    let stringToCoords [] = [] --converting "rest" coordinate string to Board of coordinates
        stringToCoords (fs:sn:rest) = (read fs::Int, read sn::Int) : stringToCoords rest 
    let newboard = stringToCoords board
    putStrLn("File loaded.")
    --assigning rules depending on if s or b comes first:
    if(s == "s") then let rules = [read s1::Int, read s2::Int, read b1::Int, read b2::Int] 
                            in game (readBoardToMatrix (square (read n::Int)) (newboard)) rules [newboard]
        else if(s == "b") then let rules = [read b1::Int, read b2::Int, read s1::Int, read s2::Int]  
                            in game (readBoardToMatrix (square (read n::Int)) (newboard)) rules [newboard]
        else error "Unknown rule assignment."

--Main method for starting new game or loading game:
main = do
    putStrLn("Enter \"c n\" to start new game (matrix nxn), or \"r 'filename'\" to load game.") 
    input <- getLine
    if ((head (input)) == 'c') then do --start game
                                    game (square (read (tail input)::Int)) [2, 3, 3, 3] []
    else if ((head (input)) == 'r') then do
                                        f <- readFile (words input!!1)
                                        loadgame f
    else putStrLn("test")
    
--Function for playing the game
game :: Matrix -> [Int] -> [Board] -> IO()
game matrix rules boards = do 
                 --cls --clear screen for each generation if desired
                 --Show matrix and read input from user
                 visMatrise matrix
                 putStrLn("Input (q/?/p/w */r */CR/l x/n x y/d x y/s s1 s2/b b1 b2): ") 
                 input <- getLine
                 --reading rules
                 let s1 = rules!!0
                 let s2 = rules!!1
                 let b1 = rules!!2
                 let b2 = rules!!3
                 --interpreting input:
                 if input == "q" then do --quit game
                                        putStrLn("Thanks for playing!") 
                                        return() 
                 else if input == "?" then do --output rules
                                            putStrLn("s " ++ show s1 ++ " " ++ show s2 ++ " b " ++ show b1 ++ " " ++ show b2)
                                            game matrix rules boards
                 else if input == "p" then do --output previous generation/board/matrix
                                            game (readBoardToMatrix (square (length matrix)) (head (tail boards))) rules (tail boards)
                 else if (head(words input) == "w") then do --save to file
                                                            --Converting alive cells to a string:
                                                            let board = readMatrixToBoard(matrix)
                                                            let coordstring [] = []
                                                                coordstring (p:ps) = " " ++ show(fst p) ++ " " ++ show(snd p) ++ coordstring(ps)
                                                            --writing to file:
                                                            writeFile ((words input)!!1) --filename
                                                                      ("s " ++ show s1 ++ " " ++ show s2 ++ " b " ++ show b1 ++ " " ++ show b2 ++ " " ++ --rules
                                                                      show (length matrix) ++ --matrix dimension
                                                                      coordstring board) --coordinates of live cells
                                                            putStrLn("File saved.")
                 else if (head(words input) == "r") then do --read board
                                                            f <- readFile (words input!!1)
                                                            loadgame f

                                                            
                 else if (input == "CR") then do --next generation
                                                let board = readMatrixToBoard(matrix) --reading the board from matrix
                                                let newboard = nextgen board s1 s2 b1 b2 (length matrix) --creating the next gen. board
                                                game (readBoardToMatrix (square (length matrix)) newboard) rules (newboards newboard boards)  --creating a "new game" from the new board
                 else if (input == "l x") then do --autoplay
                                                let f m = do --as above with: wait function and checking if previous board is same as new:
                                                    let board = readMatrixToBoard(m)
                                                    let newboard = nextgen board s1 s2 b1 b2 (length m)
                                                    let newm = readBoardToMatrix (square (length m)) newboard
                                                    wait 1000000
                                                    --with clear screen:
                                                    --if(board /= newboard) then cls >> visMatrise newm >> f newm else putStrLn("Stable configuration reached.") 
                                                    --without clear screen:
                                                    if(board /= newboard) then visMatrise newm >> f newm else putStrLn("Stable configuration reached.") 
                                                f matrix
                 else
                    do
                 --Handling input with digits (changing rules, cells etc)
                    let newmatrix = matrix
                    let (a:b:c) = words input
                    let x = (read b :: Int) - 1
                    let y = (read(head c) :: Int) - 1
                    if ((x < length matrix) && y < length (head matrix) && x >= 0 && y >=0) then --check that digits are within range of board
                        if a == "n" then let newmatrix = updateMatrix matrix 'X' (x,y) in game newmatrix rules boards--new live cell
                        else if a == "d" then let newmatrix = updateMatrix matrix '.' (x,y) in game newmatrix rules boards--update cell to dead
                        else if a == "s" then game newmatrix [x+1,y+1,b1,b2] boards --change rules for survive    
                        else if a == "b" then game newmatrix [s1,s2,x+1,y+1] boards--change rules for born    
                        else error "invalid input"
                    else do putStrLn ("Indices outside board") 
                            game matrix rules boards