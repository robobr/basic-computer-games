{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Wno-implicit-prelude #-}

import Data.Bits (shiftR, xor)
import Data.Char (toLower)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Read (readMaybe)

-- | Data types
--
-- Some fundamental data structures for game play, cards, and the money

-- A hand is 3 cards, dealt together, 2 up, 1 down
data Hand = Hand
  { lo :: Int, -- low card
    hi :: Int, -- high card
    z :: Int -- 3rd card
  }
  deriving (Eq, Show)

-- Game is the state of play and any capabilities the game needs.
data Game = Game
  { dealer :: IO Int, -- Card dealing is a capability
    hand :: Hand,
    purse :: Float,
    bet :: Float
  }

-- | Functions for random numbers and simulated BASIC-like behaviors
--
-- As one would expect, Haskell and BASIC differ on treatment of data.  These
-- functions bridge that gap without fully giving in to BASIC's neolithic ways.

-- Pseudo Random Number Generation isn't included with a plain ghc
-- install. This PRNG is based on splitmix 64-bit.
nextRnd :: Word64 -> (Word64, Word64)
nextRnd !s0 =
  let !s1 = s0 + 0x9E3779B97F4A7C15
      !z1 = (s1 `xor` (s1 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      !z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
      !s2 = z2 `xor` (z2 `shiftR` 31)
   in (s2, s1)

-- mkDealer returns a function closed over (for capturing) the PRNG state.  The
-- retruned function is used to deal a card.
mkDealer :: Word64 -> IO (IO Int)
mkDealer initialSeed = do
  seedRef <- newIORef initialSeed
  pure $ atomicModifyIORef' seedRef $ \s ->
    let (randVal, s') = nextRnd s
        toCard w = fromIntegral (w `mod` 13) + 2
     in (s', toCard randVal)

-- drawHand deals the 3 cards and ensure the first 2 (up cards) are not equal
drawHand :: IO Int -> IO Hand
drawHand dealer = do
  x <- dealer
  y <- dealer
  z <- dealer
  if x == y
    then drawHand dealer
    else do
      let (lo, hi) = if x < y then (x, y) else (y, x)
      pure (Hand lo hi z)

-- tab returns the given number of spaces
tab :: Int -> String
tab n = replicate n ' '

-- showCard does print formatting for card values
showCard :: Int -> String
showCard 11 = "JACK"
showCard 12 = "QUEEN"
showCard 13 = "KING"
showCard 14 = "ACE"
showCard n = " " ++ show n -- a leading space for a quirk of BASIC number format

-- showNum does BASIC-like print formatting for numbers
showNum :: Float -> String
showNum x
  | x == fromIntegral n = show n
  | otherwise = show x
  where
    n = round x :: Int

-- input does BASIC-like user input
input :: String -> IO Float
input prompt = do
  putStr (prompt ++ "? ")
  line <- getLine
  case readMaybe line of
    Just x -> return x
    Nothing -> do
      putStrLn "!NUMBER EXPECTED - RETRY INPUT LINE"
      input ""

-- | Game logic
--
-- Each function handles one point (state) of game play. Prints some info,
-- possibly reads user input, and dispatches to the next state.

-- main is just like any other main only more so
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- no buffering so prompts are synchronized
  dealer <- mkDealer . fst . nextRnd =<< getMonotonicTimeNSec
  let g = Game {dealer = dealer, purse = 100, hand = Hand 0 0 0, bet = 0}
  showTitleCard
  showPurse g

showTitleCard :: IO ()
showTitleCard = do
  putStrLn $ tab 26 ++ "ACEY DUCEY CARD GAME"
  putStrLn $ tab 15 ++ "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
  putStrLn ""
  putStrLn ""
  putStrLn "ACEY-DUCEY IS PLAYED IN THE FOLLOWING MANNER "
  putStrLn "THE DEALER (COMPUTER) DEALS TWO CARDS FACE UP"
  putStrLn "YOU HAVE AN OPTION TO BET OR NOT BET DEPENDING"
  putStrLn "ON WHETHER OR NOT YOU FEEL THE CARD WILL HAVE"
  putStrLn "A VALUE BETWEEN THE FIRST TWO."
  putStrLn "IF YOU DO NOT WANT TO BET, INPUT A 0"

showPurse :: Game -> IO ()
showPurse g = do
  putStrLn $ "YOU NOW HAVE  " ++ showNum g.purse ++ " DOLLARS.\n"
  dealCards g

dealCards :: Game -> IO ()
dealCards g = do
  h <- drawHand g.dealer
  putStrLn "HERE ARE YOUR NEXT TWO CARDS: "
  putStrLn $ showCard h.lo
  putStrLn $ showCard h.hi
  takeBet g {hand = h}

takeBet :: Game -> IO ()
takeBet g = do
  putStrLn ""
  b <- input "WHAT IS YOUR BET"
  showdown g {bet = b}

showdown :: Game -> IO ()
showdown g
  | g.bet == 0 = do
      putStrLn "CHICKEN!!\n"
      dealCards g
  | g.bet > g.purse = do
      putStrLn "SORRY, MY FRIEND, BUT YOU BET TOO MUCH."
      putStrLn $ "YOU HAVE ONLY  " ++ show g.purse ++ "  DOLLARS TO BET.\n"
      b <- input "WHAT IS YOUR BET"
      showdown g {bet = b}
  | (g.hand.z > g.hand.lo) && (g.hand.z < g.hand.hi) = do
      putStrLn $ show g.hand.z -- winner
      putStrLn "YOU WIN!!"
      showPurse g {purse = g.purse + g.bet}
  | otherwise = do
      putStrLn $ show g.hand.z -- loser
      putStrLn "SORRY, YOU LOSE"
      let g' = g {purse = g.purse - g.bet}
      if g'.purse > 0
        then showPurse g'
        else playAgain g'

playAgain :: Game -> IO ()
playAgain g = do
  putStrLn "\n\nSORRY, FRIEND, BUT YOU BLEW YOUR WAD."
  putStr "\n\nTRY AGAIN (YES OR NO)? "
  a <- getLine
  case map toLower a of
    "yes" -> putStrLn "\n" >> dealCards g {purse = 100}
    _ -> putStrLn "\n\nO.K., HOPE YOU HAD FUN!"
