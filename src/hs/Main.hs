module Main where

import Ffi
import Data.Char
import System.Random (randomIO)
import Data.Vector as V(Vector, generate, update, (//), (!), imap)
import Control.Monad (sequence)

width, height, range, bombs :: Int
range = width*height
width = 30
height = 30
bombs = 100

heightS :: String
heightS = "\x1b[" ++ show (height+1) ++ "A"

inBounds :: Cursor -> Bool
inBounds (x, y) =
  0<=x && x<width &&
  0<=y && y<height
    
neighbours :: Int -> [Int]
neighbours n  =
  [(y0+y)*width+(x0+x) | x<-[(-1)..1], y<-[(-1)..1], inBounds (x0+x, y0+y), x/=0 || y/=0]
  where
    y0 = div n width
    x0 = n - y0*width

neighboursC :: [Int] -> Int -> [Int]
neighboursC xs = (++) xs . filter (not . flip elem xs) . neighbours

data Kind = Empty | Bomb deriving Eq
data Dir = R | D | L | U
data State = Open | Closed | Flag deriving Eq
data Outcome = Win | Loose | Neutr
data Cell = Cell { state::State, kind::Kind, neigh::Int}

type Grid = Vector Cell
type Cursor =(Int, Int)
data World = World {grid::Grid, cursor::Cursor}

showF :: Int -> String
showF 1 = "\x1b[38;5;26m1\x1b[0m"
showF 2 = "\x1b[38;5;28m2\x1b[0m"
showF 3 = "\x1b[38;5;160m3\x1b[0m"
showF 4 = "\x1b[38;5;19m4\x1b[0m"
showF n = show n

instance Show Cell where
  show (Cell Flag   _     _) = "?"
  show (Cell Closed _     _) = "."
  show (Cell Open   Bomb  _) = "X"
  show (Cell Open   Empty x)
    | x == 0    = " "
    | otherwise = showF x
 
instance Show World where
  show (World g (x,y)) = fst $ foldl f ("",0) $ g
    where
      pos = y*width + x
      
      f :: (String, Int) -> Cell -> (String, Int)
      f (acc, i) next =
        let
          border = i/=0 && (mod i width)==0
          newLine = if border then "\n" else ""
          [op, cl] = if i==pos then "[]" else "  "
        in (acc ++ (newLine ++ [op]) ++ (show next) ++ [cl], i+1)

moveD :: Cursor -> Dir -> Cursor
moveD (x,y) R = (x+1,y)
moveD (x,y) D = (x,y+1)
moveD (x,y) L = (x-1,y)
moveD (x,y) U = (x,y-1)

isBomb :: Cell -> Int
isBomb (Cell _ Bomb _ ) = 1
isBomb _ = 0

initWorld :: [Int] -> World
initWorld ps = World g'' (0,0)
  where
    g   = generate range (\x -> Cell Closed Empty 0)
    g'  = updateGrid g ps (\c -> c {kind=Bomb})
    g'' = imap f g'

    f = \i x -> x { neigh = sum $ map (\y -> isBomb $ g' ! y) $ neighbours i}

updateGrid :: Grid -> [Int] -> (Cell -> Cell) -> Grid
updateGrid g xs f = (//) g [(x, f (g ! x)) |  x<-xs]

mark :: World -> World
mark world@(World g (x, y))
  | flag == Flag = world{grid=updateGrid g [pos] (\c -> c {state=Closed})}
  | flag == Closed = world{grid=updateGrid g [pos] (\c -> c {state=Flag})}  
  | otherwise = world
  where
    pos = y*width+x
    (Cell flag _ _) = g ! pos

insertUq :: Eq a => [a] -> a -> [a]
insertUq [] y = [y]
insertUq (x:xs) y
  | x == y = (x:xs)
  | otherwise = x : insertUq xs y

(+++) :: Eq a => [a] -> [a] -> [a]
(+++) = foldl insertUq

concatUq :: Eq a => [[a]] -> [a]
concatUq xs = foldl (+++) [] xs

revealAll :: World -> [Int] -> World
revealAll world@(World g _) xs
  | l==0 = world
  | otherwise = world'
  where 
    (set, _, l) =
      foldl (\(all, i, j) (Cell _ kind k) ->
               if kind==Empty && k==0 then (neighboursC all i, i+1, j+1)
                                      else (all,i+1, j)) ([], 0, 0) g
    world' = world {grid=updateGrid g set (\c -> c {state=Open})}

reveal :: World -> (World, Outcome)
reveal world@(World g (x,y))
  | kind == Bomb       = (openWorld, Loose)
  | n == (range-bombs) = (openWorld, Win)
  | otherwise          = (world'', Neutr)
  where
    pos = y*width+x
    (Cell _ kind k) = g ! pos

    n         = foldl (\acc (Cell state _ _) -> if state==Open then acc+1 else acc) 0 $ grid world''
    openWorld = world {grid=updateGrid g [0..(range-1)]
                     (\c@(Cell state kind _) -> if state==Flag && kind/=Bomb then c else c {state=Open})}
    world'    = world {grid=updateGrid g set (\c -> c {state=Open})}
    world''   = revealAll world' [pos]
      
    set       = if k == 0 then pos : neighboursC [] pos else [pos]
    
move :: Dir -> World -> World
move d world = world {cursor = c'}
  where c' = if inBounds t then t else c
        t  = moveD c d
        c  = cursor world

gameLoop :: World -> IO ()
gameLoop world@(World c g) = do
  putStrLn heightS
  putStrLn (show world)
  x <- getHiddenChar
  case ord x of
    27 -> return ()
    119 -> gameLoop $ move U world
    100 -> gameLoop $ move R world
    97  -> gameLoop $ move L world
    115 -> gameLoop $ move D world
    102 -> gameLoop $ mark world
    13  -> case reveal world of
      (world', Win) -> do
        putStrLn heightS
        putStrLn (show world')
        putStrLn "You won! Congratulations!"
        return ()
      (world', Loose) -> do
        putStrLn heightS
        putStrLn (show world')
        putStrLn "Oops"
        return ()
      (world', Neutr)  -> gameLoop world'
    _   -> gameLoop world

randomBombs :: IO [Int]
randomBombs = do
  xs <- sequence $ take bombs $ repeat randomIO
  return $ map (\x -> mod x range) xs

main :: IO ()
main = do
  ansi_enable
  indexes <- randomBombs
  let world = initWorld indexes
  putStrLn $ show world
  gameLoop world
  ansi_disable
