module AI where

import Board
import Data.List

getChoice board player = 1


getComputerChoice board player depth = if depth == 0
                                        then evaluate board 


evaluate board =

evaluateVer board player = sum [evaluateColumn column player | column <- board]


evaluateColumn column player = 

opponent 1 = 2
opponent _ = 1

checkFour column pos player = let a = splitAt pos column
                                in if opponent player `elem` a
                                    then 0
                                    else if length (elemIndices player a) == 4
                                          then 1000
                                          else 0

checkThree column pos player = let a = splitAt pos column
                                in if opponent player `elem` a
                                    then 0
                                    else if length (elemIndices player a) == 3
                                          then 100
                                          else 0

checkTwo column pos player = let a = splitAt pos column 
                              in if opponent player `elem` a
                                  then 0
                                  else if length (elemIndices player a) == 2
                                        then 10
                                        else 0

