module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse, (\\))
import System.Exit (exitSuccess)
import System.Random (randomRIO)


-- Extras
numGuesses :: Int
numGuesses = 9

-- p 1
-- type WordList = [String]
newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

-- p 2
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9


gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in      l >= minWordLength
              &&  l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl - 1))
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


-- p3
data Puzzle = Puzzle String [Maybe Char] [Char]

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle "" = error "No word was given"
freshPuzzle xs = Puzzle xs template []
  where template =
          map (const Nothing) xs

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle p _ _) c = elem c p

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c)
            word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick \
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return (fillInCharacter puzzle guess)

getRemainingGuesses :: Puzzle -> Int
getRemainingGuesses (Puzzle _ filledInSoFar guessed) =
  if length guessed == 0
  then numGuesses
  else
    numGuesses - length incorrectGuesses
    where
      revealedSoFar = map (\(Just x) -> x) . filter isJust $ filledInSoFar
      incorrectGuesses = guessed \\ revealedSoFar

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle wordToGuess filledInSoFar guessed) =
  if getRemainingGuesses p == 0 then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStrLn $
    "Remaining guesses: " ++ show  (getRemainingGuesses puzzle)
  putStr "Guess a Letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   ->
      putStrLn "Your guess must\
              \ be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle

