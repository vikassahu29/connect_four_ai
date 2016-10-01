module Main where

import Data.Maybe
import Board
import AI

main :: IO ()
main = do
  putStrLn "Choose game format:"
  putStrLn "1: Player vs Player"
  putStrLn "2: Player vs Computer"
  putStrLn "3: Computer vs Computer"

  choice <- getLine
  processChoice $ read choice


processChoice :: Int -> IO ()
processChoice 1 = do
                    putStrLn "2 Player Game"
                    start2PlayerGame initBoard 1
processChoice 2 = do 
                    putStrLn "1 Player Game"
                    start1PlayerGame initBoard 1
processChoice 3 = do 
                    putStrLn "0 Player Game"
                    start0PlayerGame initBoard 1


start2PlayerGame a player = do 
                              displayBoard a
                              putStrLn "Choose Column"
                              column <- getLine
                              processTurn start2PlayerGame a player (read column)

start1PlayerGame a player = do
                              displayBoard a
                              putStrLn "Choose Column:"
                              column <- getInput a player
                              processTurn start1PlayerGame a player (read column)

start0PlayerGame a player = do
                              displayBoard a
                              putStrLn "Choose Column:"
                              processTurn start0PlayerGame a player $ getInput0 a player

getInput a player = if player == 1
                      then getLine
                      else return $ show $ getComputerChoice a 2 2

getInput0 a player = if player == 1
                      then getComputerChoice a 1 2
                      else getComputerChoice a 2 4

processTurn gameFunc board player column = if column > 6
                                            then do 
                                                  putStrLn "Invalid Move"
                                                  gameFunc board player
                                            else let nBoard = makeBoardMove board column player
                                                  in if nBoard == Nothing
                                                      then do
                                                        putStrLn "Invalid Move"
                                                        gameFunc board player
                                                      else let terminal = isTerminal (fromJust nBoard)
                                                        in if terminal == Nothing
                                                            then gameFunc (fromJust nBoard) (opponent player)
                                                            else do
                                                                  displayBoard (fromJust nBoard)
                                                                  if terminal == Just 0
                                                                    then putStrLn "Draw"
                                                                    else do
                                                                          putStr "Player "
                                                                          putStr $ show $ fromJust terminal
                                                                          putStr " Wins\n"
