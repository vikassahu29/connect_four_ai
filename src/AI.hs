module AI where

import Board
import Data.List
import Data.Maybe

getComputerChoice board player maxDepth = fst (getComputerChoice' board player 0 True maxDepth)

getComputerChoice' board player depth isMax maxDepth
  | depth == maxDepth = (-1, evaluate board player)
  | otherwise = let arr = [ a | x <- [0..6], let a = randomFunc board x player depth isMax maxDepth]
                 in if isMax
                      then let max = maximum arr in (findInList max arr, max)
                      else let min = minimum arr in (findInList min arr, min)
                  where findInList x xs = let xI = findIndex (\a -> a == x) xs
                                            in if xI == Nothing
                                                then -1
                                                else fromJust xI

randomFunc board x player depth isMax maxDepth = let pos = makeBoardMove board x player
                              in if pos == Nothing
                                  then if isMax then -100000 else 100000
                                  else snd (getComputerChoice' (fromJust pos) (opponent player) (depth + 1) (not isMax) maxDepth)

evaluate :: (Num a1, Num a, Eq a1, Eq a) => [[a1]] -> a1 -> a
evaluate board player = (evaluateVer board player) + (evaluateHor board player)


evaluateHor board player = sum [evaluateRow row player | row <- getRows board ]

getRows board = [ [getMatrixElem board i j | i <- [0..(xLen - 1)] ] | j <- [0..(yLen - 1)]]
                where xLen = length board
                      yLen = length $ board !! 0

evaluateVer :: (Num a1, Num a, Eq a1, Eq a) => [[a1]] -> a1 -> a
evaluateVer board player = sum [evaluateRow column player | column <- board]

evaluateRow :: (Num a1, Num a, Eq a1, Eq a) => [a1] -> a1 -> a
evaluateRow row player = (checkFour row player) - (checkFour row $ opponent player)

opponent 1 = 2
opponent _ = 1

checkFour :: (Num a1, Num a, Eq a1, Eq a) => [a1] -> a1 -> a
checkFour row player = sum [ getEvaluation (snd $ (splitAt pos row)) player | pos <- [0.. ((length row) - 4) ]]


getEvaluation row player = failOver verifyFour verifyThree verifyTwo verifyOne row player

failOver :: (Num t2, Eq t2) => (t -> t1 -> t2) -> (t -> t1 -> t2) -> (t -> t1 -> t2) -> (t -> t1 -> t2) -> t -> t1 -> t2 
failOver f1 f2 f3 f4 row player = if f1Res /= 0
                                then f1Res
                                else if f2Res /= 0
                                      then f2Res
                                      else if f3Res /= 0
                                            then f3Res
                                            else f4Res
                                where f1Res = f1 row player
                                      f2Res = f2 row player
                                      f3Res = f3 row player
                                      f4Res = f4 row player
                                

verifyFour :: (Num a, Num t, Eq a) => [a] -> a -> t
verifyFour row player
  | checkOpponent row player = 0
  | length (elemIndices player row) == 4 = 10000
  | otherwise = 0

verifyThree :: (Num a, Num t, Eq a) => [a] -> a -> t
verifyThree row player 
  | checkOpponent row player = 0
  | length (elemIndices player row) == 3 = 100
  | otherwise = 0

verifyTwo :: (Num a, Num t, Eq a) => [a] -> a -> t
verifyTwo row player
  | checkOpponent row player = 0
  | length (elemIndices player row) == 2 = 10
  | otherwise = 0

verifyOne :: (Num a, Num t, Eq a) => [a] -> a -> t
verifyOne row player
  | checkOpponent row player = 0
  | length (elemIndices player row) == 1 = 1
  | otherwise = 0

checkOpponent row player = opponent player `elem` row
