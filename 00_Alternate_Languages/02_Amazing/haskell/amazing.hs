{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Wno-implicit-prelude #-}

import Control.Monad (unless)
import Data.Bits (shiftR, xor)
import Data.Ix (inRange)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
-- import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- | Data types
--
-- The fundamental data structures and constructors

-- Cell is aka cell aka a single (x,y) position in the maze grid
type Cell = (Int, Int)

-- CellSet is a collection of unique nodes
type CellSet = Set.Set Cell

-- Link is a bidirectional path between two nodes, i.e. the open walls
type Link = (Cell, Cell)

-- Passages is a collection of unique links, i.e. the list of all open walls
type Passages = Set.Set Link

-- PrngState is the 64-bit state of the PRNG
type PrngState = Word64

-- | mkLink creates an edge such that (a,b) = (b,a) (the lesser value is first)
mkLink :: Cell -> Cell -> Link
mkLink a b = if a < b then (a, b) else (b, a)

-- | Functions for random numbers and simulated BASIC-like behaviors
--
-- As one would expect, Haskell and BASIC differ on treatment of data.  These
-- functions bridge that gap without fully giving in to BASIC's neolithic ways.

-- Pseudo Random Number Generation isn't included with a plain ghc
-- install. This PRNG is based on splitmix 64-bit.
nextRnd :: PrngState -> (Word64, PrngState)
nextRnd !s0 =
  let !s1 = s0 + 0x9E3779B97F4A7C15
      !z1 = (s1 `xor` (s1 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      !z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      !s2 = z2 `xor` (z2 `shiftR` 31)
   in (s2, s1)

-- rndN computes the a random number indexing the range [0, n - 1]
rndN :: Int -> PrngState -> (Int, PrngState)
rndN n s =
  let (v, s') = nextRnd s
   in (fromIntegral (v `mod` fromIntegral n), s')

-- | tab returns given the number of spaces for print formatting
tab :: Int -> String
tab n = replicate n ' '

-- | Prompt user for and Read a pair of integers.
inputPair :: String -> IO (Int, Int)
inputPair = go []
  where
    go acc rp = do
      putStr rp
      line <- getLine
      case (acc ++) <$> traverse readMaybe (words (map uncomma line)) of
        Nothing -> do
          putStrLn "!NUMBER EXPECTED - RETRY INPUT LINE"
          go acc "? "
        Just (a : b : rest) -> do
          unless (null rest) $ putStrLn "!EXTRA INPUT IGNORED"
          pure (a, b)
        Just partial -> go partial "?? "
    uncomma ',' = ' '
    uncomma c = c

-- | Prompt user and read dimensions
promptDims :: String -> IO (Int, Int)
promptDims p = do
  (h, v) <- inputPair p
  if h >= 2 && v >= 2
    then pure (h, v)
    else do
      putStrLn "MEANINGLESS DIMENSIONS.  TRY AGAIN."
      promptDims p

-- | Haskell main is like any other main only more so
main :: IO ()
main = do
  -- print the title and prompt for maze dimensions
  putStrLn $ tab 28 ++ "AMAZING PROGRAM"
  putStrLn $ tab 15 ++ "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
  putStrLn "\n\n\n"
  (w, h) <- promptDims "WHAT ARE YOUR WIDTH AND LENGTH? "

  -- seed the random number generator and generate the maze
  s0 <- getMonotonicTimeNSec
  let (_, s1) = nextRnd s0 -- seed prng with nano time and gen the real seed
      maze = generateMaze s1 w h

  -- print the maze
  putStrLn "\n\n\n"
  putStr $ showMaze w h maze

-- | generateMaze carves passages across a grid of given dimension using
-- recursive backtracking algorithm.
generateMaze :: Word64 -> Int -> Int -> Passages
generateMaze seed width height =
  go seed (Set.singleton (0, 0)) [(0, 0)] Set.empty
  where
    -- bounds is the inclusive coordinates of the maze.  we must be sure to
    -- stay inside the maze.
    bounds = ((0, 0), (width - 1, height - 1))

    -- this 'go' function is/does the recursive backtracking
    go :: Word64 -> CellSet -> [Cell] -> Passages -> Passages
    -- base case: no more cells on the stack so none left to traverse.
    go rng _ [] passages = foldr Set.insert passages [door1, door2]
      where
        -- at this point the maze is built.  all passages are known except the
        -- entrance and exit doors.  all cells are connected in this solution
        -- so we can pick any random cell to enter along to the top or exit at
        -- the bottom.
        (a, rng') = rndN width rng
        (b, _) = rndN width rng'
        door1 = mkLink (a, 0) (a, -1)
        door2 = mkLink (b, height - 1) (b, height)
    -- recursive case: pop the current cell off the stack and examine its
    -- neighbors. pick and unvisited neighbor and go there.
    go rng visited (curr : stack) passages =
      case unvisitedNeighbors curr of
        [] -> go rng visited stack passages
        -- all neighbors already visited. there's no where to go, so backtrack
        neighbors ->
          let -- take a random unvisited neighbor to visit
              (idx, rng') = rndN (length neighbors) rng
              next = neighbors !! idx
              -- add it to the list of visited cells
              visited' = Set.insert next visited
              -- push it onto the stack because it's next for consideration
              stack' = (next : curr : stack)
              -- the current cell and the next random cell now form a passage
              -- in the list of all passages.  add this link to the list of
              -- passages.
              passages' = Set.insert (mkLink curr next) passages
           in go rng' visited' stack' passages'
      where
        -- allNeighbors lists every cell in the 4 cardinal directions N,S,E,W
        -- that are withing the maze boundaries (width, height)
        allNeighbors (x, y) =
          filter
            (inRange bounds)
            [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

        -- unvisitedneighbors is allNeighbors that aren't in the visited set
        unvisitedNeighbors (x, y) =
          filter (`Set.notMember` visited) $ allNeighbors (x, y)

-- | Convert the maze into it's printable ascii format
showMaze :: Int -> Int -> Passages -> String
showMaze width height passages =
  unlines $ topLine : wallsAndFloors
  where
    wallsAndFloors = concatMap row [0 .. height - 1]
    isOpen r c = Set.member (mkLink r c) passages
    mkEntrance x = "." ++ if isOpen (x, 0) (x, -1) then "  " else "--"
    topLine = concat [mkEntrance x | x <- [0 .. width - 1]] ++ "."
    row y = [wallsLine, floorLine]
      where
        mkCellWalls x = "  " ++ if isOpen (x, y) (x + 1, y) then " " else "I"
        mkCellFloor x = ":" ++ if isOpen (x, y) (x, y + 1) then "  " else "--"
        --
        wallsLine = 'I' : concat [mkCellWalls x | x <- [0 .. width - 1]]
        floorLine = concat [mkCellFloor x | x <- [0 .. width - 1]] ++ "."
