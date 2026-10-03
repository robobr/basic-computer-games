{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Wno-implicit-prelude #-}

-- | Haskell port of Acey Ducey BASIC
module Main (main) where

import Data.Bits (shiftR, xor)
import Data.Char (toLower)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, isEOF, stdout)
import Text.Read (readMaybe)

-- | The complete game state and infinite list of random numbers.
data Game = Game {purse :: !Double, hand :: !Hand, cards :: [Int]}

-- | A complete hand with 2 up cards and 1 hole card.
data Hand = Hand {low :: !Int, high :: !Int, hole :: !Int}

-- $ Game logic
--
-- A top-level function per game state; mutually tail-call recursive

-- | Main is just like any other main only more so.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- make prompts show before reading input
  seed <- getMonotonicTimeNSec
  showTitleCard
  showPurse (deal 100 (deck seed))

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
  where
    tab n = replicate n ' '

-- | Show the purse, then show the hand.
showPurse :: Game -> IO ()
showPurse g = do
  putStrLn $ "YOU NOW HAVE  " ++ showNum g.purse ++ "  DOLLARS.\n"
  showHand g

-- | Show the hand, the 2 up cards, and take a bet.
showHand :: Game -> IO ()
showHand g = do
  putStrLn "HERE ARE YOUR NEXT TWO CARDS: "
  putStrLn (showCard g.hand.low)
  putStrLn (showCard g.hand.high)
  putStrLn ""
  takeBet g

-- | Ask for a bet and settle it.
takeBet :: Game -> IO ()
takeBet g = inputNum "WHAT IS YOUR BET" >>= settle g

-- | Settle the bet and offer next steps
settle :: Game -> Double -> IO ()
settle g b
  | b == 0 = putStrLn "CHICKEN!!\n" >> showHand (deal g.purse g.cards)
  | b > g.purse = do
      putStrLn "SORRY, MY FRIEND, BUT YOU BET TOO MUCH."
      putStrLn $ "YOU HAVE ONLY  " ++ showNum g.purse ++ "  DOLLARS TO BET.\n"
      takeBet g
  | g.hand.low < g.hand.hole && g.hand.hole < g.hand.high = do
      putStrLn (showCard g.hand.hole)
      putStrLn "YOU WIN!!"
      showPurse (deal (g.purse + b) g.cards)
  | otherwise = do
      putStrLn (showCard g.hand.hole)
      putStrLn "SORRY, YOU LOSE"
      let g' = deal (g.purse - b) g.cards
      if g'.purse > 0 then showPurse g' else playAgain g'

-- | Do you want to play again?
playAgain :: Game -> IO ()
playAgain g = do
  putStr "\n\nSORRY, FRIEND, BUT YOU BLEW YOUR WAD.\n\n\n"
  a <- inputStr "TRY AGAIN (YES OR NO)"
  if map toLower a == "yes"
    then putStrLn "\n" >> showPurse g {purse = 100}
    else putStrLn "\n\nO.K., HOPE YOU HAD FUN!"

-- $ Helper Funcs
--
-- Functions for handling input, output, and random number generation

-- | Prompt, read, and return a line of text
inputStr :: String -> IO String
inputStr prompt = do
  putStr (prompt ++ "? ")
  eof <- isEOF
  if eof
    then putStrLn "!END OF INPUT" >> exitSuccess
    else getLine

-- | Prompt, read, and return a number
inputNum :: String -> IO Double
inputNum prompt = do
  line <- inputStr prompt
  case readMaybe line of
    Just x -> return x
    Nothing -> do
      putStrLn "!NUMBER EXPECTED - RETRY INPUT LINE"
      inputNum ""

-- | Deal a new hand.
deal :: Double -> [Int] -> Game
deal p (x : y : c : rest)
  | x == y = deal p rest -- lo and hi cards must differ
  | otherwise = Game p (Hand (min x y) (max x y) c) rest
deal _ _ = error "the deck is infinite" -- this can't happen but just in case

-- | An infinite list of random cards.
deck :: Word64 -> [Int]
deck s =
  let (r, s') = nextRnd s
      card = fromIntegral (r `mod` 13) + 2
   in card : deck s'

-- | Get the next random number.  Used to supply the infinite list.
nextRnd :: Word64 -> (Word64, Word64)
nextRnd !s0 =
  let !s1 = s0 + 0x9E3779B97F4A7C15
      !z1 = (s1 `xor` (s1 `shiftR` 30)) * 0xBF58476D1CE4E5B9
      !z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
   in (z2 `xor` (z2 `shiftR` 31), s1)

-- | Format a card with it's name or number with some of the BASIC quirks
showCard :: Int -> String
showCard 11 = "JACK"
showCard 12 = "QUEEN"
showCard 13 = "KING"
showCard 14 = "ACE\n" -- bug in orig basic.  ACE prints with extra newline
showCard n = ' ' : show n

-- | Format numbers close to how BASIC prints. Omit decimal for whole numbers.
showNum :: Double -> String
showNum x
  | x == fromIntegral n = show n
  | otherwise = show x
  where
    n = round x :: Int
