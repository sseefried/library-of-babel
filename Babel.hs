module Babel where

import           Data.Char
import           Data.IORef
import           Text.Printf
import           Data.Array.IArray
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad
import           Data.Maybe

navKeys = "012345"

numNavKeys :: Integer
numNavKeys = fromIntegral $ length navKeys

numberOfCharacters = 80*40 -- same as for twitter
-- alphabetList = ' ':['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ",.!?;:'\"@#$%^&*()="

alphabetList = ' ':['A'..'Z'] ++ ",.!?;:'\"&"

alphabetLen :: Int
alphabetLen = length alphabetList

alphabetLen' :: Integer
alphabetLen' = fromIntegral alphabetLen

mapFromString :: String -> Map Char Integer
mapFromString list = foldl (\m (k,v) -> Map.insert k v m) Map.empty (zip list [0..])

alphaMap :: Map Char Integer
alphaMap = mapFromString alphabetList

-- alphabet :: Array 
alphabet :: Array Integer Char
alphabet = listArray (0, alphabetLen' - 1) alphabetList

back = ' '
navMap :: Map Char Integer
navMap = mapFromString navKeys

data Navigation = Back | Forward Integer

------------

toBase :: Int -> Integer -> String
toBase b n = reverse $ aux n 
  where
    b' = fromIntegral b
    aux n | n > 0     = (series !! (fromIntegral (n `mod` b'))) : aux (n `div` b')
          | otherwise = ""
    series = ['0'..'9'] ++ ['a'..'z']

textToBabel s = (sequence . aux . reverse $ s) >>= \r -> return (foldl (\n m -> n*alphabetLen' + m) 0 r)
  where aux s = map (\c -> Map.lookup c alphaMap) s

babelToText :: Integer -> String
babelToText n = aux n
  where 
  	aux n | n > 0 = let r = n `mod` alphabetLen'
                           in  alphabet ! r : aux (n `div` alphabetLen')
          | otherwise = ""

poem :: Integer
poem = 0x1ca181b5a983edee937a71f5c107d73e328ed3a46fd6ecd3cb331686848efb1924fd5f66d8d61c46bd66171f68f6b6e907d78b967901af7caa0d07ea89d350d06aa0e83e84946fe66c6f1932e022f8247

-- Just "e1ef09" Just "464175" 4604277

poemString = "T'was the night before Christmas, and all through the house not a creature was stirring, not even a mouse"

numberOfBooks = (alphabetLen')^numberOfCharacters

data State = State { address :: Integer }


keyToNav :: Char -> Maybe Navigation
keyToNav c | c == back = Just Back
           | otherwise =  case Map.lookup c navMap of
                            Nothing -> Nothing
                            Just i -> Just $ Forward i

mainLoop :: IORef State -> IO ()
mainLoop sRef = do
  s <- readIORef sRef
  putStrLn "\nYou're in a vast library of sentences. The sentence here is: "
  printf "\n%s\n" $ babelToText $ address s
  printf "\n\nWhich way? "
  c <- getChar
  modifyIORef sRef $ case keyToNav c of
    Just Back        -> \s -> State { address = address s `div` numNavKeys } 
    Just (Forward i) -> \s -> State { address = address s *  numNavKeys + i }
    Nothing          -> id

  mainLoop sRef

main :: IO ()
main = do
  sRef <- newIORef (State { address = 0 })
  mainLoop sRef

-----------

factoids = [ "This library is not infinite, although it is very large. Given enough time you could see all of it.",
             "Every thing you can think of is in this library",
             "A sentence summing up every day of your life exists in this library",
             "What's going to happen to you tomorrow is in this library",
             "A slanderous lie about you is in this library",
             "A note your mother once wrote to you is in this library",
             "Things written in languages never spoken are in this library",
             "This fact is in the library somewhere",
             "There are more phrases in this library than there are atoms in the universe",
             "You could construct a book by stringing together many sentences from this library",
             "If all books are in here, what role do writers have?",
             "What we pay writers can be though of as a 'finders fee' for finding something interesting in this library" ]
