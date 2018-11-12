-- Yana Georgieva, ComputerScience, 2 course, 1 stream, group 3, FN: 81281

-- Using zipWith, concatMap and other functions from this library.
import           Data.List
-- Found a very nifty way to change an element in matrix using some fuctions from here.
import           Control.Lens
-- Using comparing.
import           Data.Ord
-- Using random generator of Ints in an interval getStdRandom, randomR.
import           System.Random
-- To convert an IO Int into a "normal" Int.
import           System.IO.Unsafe
-- For the IO operations in main.
import           System.IO

-- The game tree structure.
data Tree a = Nil | Node a [Tree a] deriving (Show,Eq)

-- It's easier to think of it as a board.
type Board = [String]

-- The first parameter is the number of the row and the secons is the column number.
type Position = (Int,Int)
-- Finding the diagonal of the board.
-- The parameter "x" is the board.
diag :: [[a]] -> [a]
diag x = zipWith (!!) x [0..]

-- Finding the second diagonal of the board.
-- The parameter "x" is the board.
secDiag ::[[a]] -> [a]
secDiag x = zipWith (!!) x [2, 1, 0]

-- Checks if the board "b" is empty.
isEmpty :: Board -> Bool
isEmpty b = concat b == filter (== '_') (concat b)

-- Checks if the board "b" is full.
isFull :: Board -> Bool
isFull b = concat b == filter (/= '_') (concat b)

-- We generate all the empty positions. We return a list of them.
emptySpace :: Board -> [Position]
emptySpace board = [(x, y) | x <- [0 .. 2], y <- [0 .. 2], ((board !! x) !! y) == '_']

-- We check if the board is won by the current player, who is the parameter "pl".
isWon :: Char -> [String] -> Bool
isWon pl board = elem win board || elem win (transpose board)
                        || diag board == win || secDiag board == win
                            where
                                win = replicate 3 pl

-- We check if we can block an opponent move. We use it to sort the boards when all of the scores are equal.
-- It will become clearer  when we go to the genNextMove function.
-- The idea if that some moves may have equal scores, but we don't want to return the first random board.
-- That is why this ordering helps us to return a "smater" move, not the first random.
-- With it:
-- X X
--  O
--  OX
-- We will return for sure
-- XOX
--  O
--  OX
-- or
-- X X
--  OO
--  OX
-- Without it there is a chance we might return
-- X X
-- OO
--  OX
-- or
-- X X
--  O
-- OOX
isBlocked :: Board -> Int
isBlocked board = length (intersect win board) + length (intersect win (transpose board))
                        + (if elem (diag board) win then 1 else 0) + (if elem (secDiag board) win then 1 else 0)
                            where
                                win = permutations [pl, pl, other pl]
                                pl =  other (initPlayer board)

-- Changing the current player.
other :: Char -> Char
other pl = if pl == 'X' then 'O' else 'X'

-- We generate the next possible moves using the positions from emptySpace.
-- If a winning board(s) is(are) generated then the generated along them do not pose any interest to us.
doMoves :: Char -> Board -> [Board]
doMoves player board =if null check then moves else check
    where
        moves = [board & element (fst position) . element (snd position) .~ player| position <- emptySpace board]
        check = filter (\ b -> isWon player b || isWon (other player) b) moves

-- Generating the game tree.
-- The leaves are the won boards and the full boards which have come to a draw.
generate :: Board -> Char -> Tree Board
generate board player
    |isWon player board || isWon (other player) board  = Node board [Nil]
    | null (doMoves player board) =  Node board [Nil]
    |otherwise = Node board (map (\x-> generate x (other player)) (doMoves player board))

-- For the minimax algorithm given a current game board, a player and the depth of the recursion on the current step of the scan of the already generated game tree.
makeScore :: Char -> Board -> Int -> Int
makeScore  player board depth
    | isWon player board = 10 - depth -- If the current player wins we give a positive score.
    | isWon (other player) board = depth - 10 -- If the opponent player wins we give a negative score.
    | otherwise = 0 -- If it is a draw we give a 0.

-- We map the makeScore function to each board and that is how we generate the score tree.
-- Here the parameters are the game tree, the first player and the depth of the recursive scan.
-- We keep   track of the recursive scan so we can give a more realistic score.
mapTree :: Tree Board -> Char -> Int -> Tree Int
mapTree Nil _ _ = Nil
mapTree (Node board []) player depth =  Node (makeScore player board depth) []
mapTree (Node board (x1:y:xs)) player depth = Node (makeScore player board depth) (map (\ x -> mapTree x (other player) (depth + 1)) (x1:y:xs))
mapTree (Node board [boards]) player depth = Node (makeScore player board depth) (map (\ x -> mapTree x (other player) (depth + 1)) [boards])

-- We gather the leaves of the tree (the Tree of scores) and return them as a list.
getLeaves :: Eq a => Tree a -> [a]
getLeaves Nil      = []
getLeaves (Node curr []) = [curr]
getLeaves (Node curr children) = if all ( == Nil) children
                                then [curr]
                                else concatMap getLeaves children

-- We get the maximum element of the leaves of the tree of scores.
getMax :: Tree Int ->  Int
getMax inttree = maximum (getLeaves inttree)

-- The main function which takes the initial player and the board.
-- We handle the times when the board is full or already won by any of the players.
-- If the board is empty we generate the next board with the X put at the 4 random corners of the old empty one.
-- Otherwise we take the board with the best score generated by minimax algorithm  and the best blocking score.
genNextMove :: Char -> Board -> Board
genNextMove player board
    | isFull board = ["Sorry!","Full board!"]
    | isWon 'O' board = ["Sorry!","O already won the board!"]
    | isWon 'X' board = ["Sorry!","X already won the board!"]
    | initPlayer board == 'N' = ["Not valid table.", "Sorry!", "Try again! :)"]
    | isEmpty board = (doMoves player board ) !! ( [2,4,6,8] !! (unsafePerformIO (getStdRandom (randomR (0, 3)))))
    | otherwise = fst (maximumBy (comparing snd)  (sortBy (flip (comparing (isBlocked . fst))) [(game, getMax (mapTree (generate game (other player)) (other player) 0)) | game <- doMoves player board]))

-- We see which players turn it is.
-- If the board is invalid we return a 'N' which is an error message.
-- X always starts first.
initPlayer :: Board -> Char
initPlayer board
    | count board 'X' == count board 'O' = 'X'
    | count board 'X' - count board 'O' == 1 = 'O'
    | otherwise = 'N'
    where
        count::[String] -> Char -> Int
        count str ch = length (filter (== ch) (concat str))

-- The function I use to mask that genNextMove takes a second parameter - the initial player
generateMove :: Board -> Board
generateMove board = genNextMove (initPlayer board) board

-- Helps IO printing of the board in main
makePresentable :: Board -> String
makePresentable = unlines

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Enter a board please: \n"
    line1 <- getLine
    line2 <- getLine
    line3 <- getLine
    putStrLn ("result:\n" ++ makePresentable(generateMove [line1,line2,line3]))
    putStr "Do you want to continue with another board?\nIf yes enter \"yes\": \n"
    line <- getLine
    if line == "yes" then main
    else putStr "The end!\nThanks for playing :)\n"

--My tests
--main :: IO()
--main = do
    --print(generate ["X  ", " O ", "O X"] 'O')
    --print(doMoves  'O' ["X  ", " OX", "O X"] )
    --print((generate  ["X  ", " O ", "O X"] 'X'))
    --print(mapTree (generate ["XX ", "OOX", "O X"] 'O') 'O' 0)
    --print( (getLeaves (mapTree (generate  ["X  ", " O ", "OXX"] 'O') 'O' 0)))
    --print( (getLeaves (generate  ["X  ", " O ", "OXX"] 'O')))
    --print(nodesAtLevel 2  (generate  ["X  ", " O ", "OXX"] 'O'))
    --print(genNextMove 'O' ["X X"," OX","O O"])
    --print(genNextMove 'O' ["OXX","X  ","XOO"])
    --print(genNextMove 'O' [" X ","  X","OOX"])
    --print(genNextMove 'X' ["O X","X  ","XOO"])
    --print(genNextMove 'X' ["X  "," O ","O X"])
    --print(genNextMove 'O' ["O X","  X","XOO"])
    --print(genNextMove 'X' ["   ","   ","   "])
    --print(genNextMove 'X' ["X  "," O ","O X"])
    --print(generateMove ["OXO"," X "," OX"])
    --print(generateMove ["X X"," OX","O O"])
    --print(generateMove ["OXX","X  ","XOO"])
    --print(generateMove [" X ","  X","OOX"])
    --print(generateMove ["O X","X  ","XOO"])
    --print(generateMove ["X X"," O ","O X"])
    --print(generateMove ["O X","  X","XOO"])
    --print(generateMove ["XOX","XOX","OXO"])
    --print(generateMove ["X  "," O ","   "])
    --print( randomRIO (0, 3))
