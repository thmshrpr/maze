import Data.List (intercalate)
import qualified Data.Set as S
import System.Random
import System.Environment(getArgs)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- An alias for a block in a Maze as a tuple represented a coordinate.  
type Block = (Int, Int)

-- A represenation of a Maze as a height, width, and two sets of walls.  The Northern
-- and Eastern walls can be represented as the Southern and Western Walls of adjacent
-- blocks
data Maze = Maze Int -- Width
                 Int -- Height
                 (S.Set Block) -- South walls
                 (S.Set Block) -- West walls

--------------------------------------------------------------------------------
-- Main execution function
--------------------------------------------------------------------------------
-- Main execution function, takes care of side-effectful setup (parsing 
-- commandline arguments, RNG setup) and calls the main function.  Not very
-- tolerant to bad args (e.g. non-numeric args)

main :: IO ()
main = do 
  args <- getArgs
  g <- getStdGen
  let rs = randoms g
  case args of
    [h, w]       -> print (generateMaze rs (read h) (read w) 0 0)
    [h, w, x, y] -> print (generateMaze rs (read h) (read w) (read x) (read y))
    xs           -> error "Invalid commandline arguments"
  

--------------------------------------------------------------------------------
-- Maze Generation
--------------------------------------------------------------------------------

-- This function initialises a maze generator that uses a depth first search. Its
-- first argument is an infinite, lazy list of numbers used to pick neighbours.  This
-- is a common way to utilise randomness in pure functions, as System.Random can
-- can provide precisely such a list.  The arguments h and w are the height and width, 
-- respectively, of the resulting maze.  The arguments x and y are the starting 
-- coordinates for the generation.  If they lay outside the maze, a "fully saturated"
-- maze will result.
generateMaze :: [Int] -> Int -> Int -> Int -> Int -> Maze
generateMaze rs h w x y = dfs rs (S.singleton (x,y)) [(x,y)] (fullMaze h w)

-- A depth-first search for creating a maze.  It uses a set to keep track of visited
-- nodes.  A stack (represented as a list) is used for backtracking.  The current
-- node is actually the head of the list, which gives the pleasing termination 
-- condition of an empty list.  

-- The algorithm works by identifying unvisited neighbours of the current block, 
-- randomly selecting one, and then removing the wall between them. The randomly 
-- selected neighbour then becomes the current block and the original is pushed onto.
-- the stack.  When a block has no neighbours, the first cell on the stack is popped
-- and becomes the current cell.  
dfs :: [Int] -> S.Set Block -> [Block] -> Maze -> Maze
dfs _ _ [] m = m
dfs (r:rs) visited (cur@(x,y):stack) (Maze h w souths wests) 
    | [] <- neighbours = dfs (r:rs) visited stack (Maze h w souths wests)  
    | zs <- neighbours = 
            let rn = zs !! (r `mod` length zs)
                (souths', wests') = removeWall cur rn souths wests
                visited'          = S.insert rn visited
            in  dfs rs  visited' (rn:(cur:stack)) (Maze h w souths' wests') 
    where
      neighbours   = filter (\x -> inMaze x && not (S.member x visited)) 
                       [(x, y+1),(x,y-1),(x+1, y),(x-1, y)]
      inMaze (a,b) = a >= 0 && a < h && b >= 0 && b < w
      -- The Applicative programmer might write     
      --  filter ((&&) <$> inMaze <*> (not . (flip S.member) visited))
      --                 [(x, y+1),(x,y-1),(x+1, y),(x-1, y)]
      -- ;) 

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- Generates a "fully saturated" maze where all blocks are surrounded by four walls. 
-- This serves as the started point for generating a navigable maze by taking away
-- walls.
fullMaze :: Int -> Int -> Maze
fullMaze x y = Maze x y souths wests
    where
      souths = (S.fromList [(m, n) | m <- [0..x-1], n <- [0..y]])
      wests  = (S.fromList [(m, n) | m <- [0..x], n <- [0..y-1]])

-- Assumption: the two input blocks are adjacent
removeWall :: Block -> Block -> S.Set Block -> S.Set Block 
           -> (S.Set Block, S.Set Block)
removeWall b1@(x1,y1) b2@(x2,y2) souths wests 
    | x1 < x2   = (souths, S.delete b2 wests)  --b1 is west of b2
    | x1 > x2   = (souths, S.delete b1 wests)  --b1 is east of b2
    | y1 < y2   = (S.delete b2 souths, wests)  --b1 is south of b2
    | y1 > y2   = (S.delete b1 souths, wests)  --b1 is north of b2
    | otherwise = (souths, wests) --same block, no-op, included for completeness

-- Provide an instance of the Show typeclass for displaying Mazes.  This lazily 
-- generates a list of every coordinate in the Maze and draws them from the top-left.
instance Show Maze where 
    show (Maze h w souths wests) = 
        intercalate "\n" $ map (concatMap (\ c -> westWall c ++ southWall c)) 
                                  (generateGrid h w)
        where
          westWall c = if S.member c wests 
                       then "|"
                       else " " 
          southWall c = if S.member c souths
                        then "_"
                        else " "

generateGrid x y = [ generateRow x n | n <- [y,y-1..0]]
generateRow x y  = [(n,y) |  n <- [0..x]]