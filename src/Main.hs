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
processChoice 2 = putStrLn "1 Player Game"
processChoice 3 = putStrLn "0 Player Game"


start2PlayerGame a player = do 
                              displayBoard a
                              putStrLn "Choose Column"
                              column <- getLine
                              processTurn start2PlayerGame a player (read column)

start1PlayerGame a player = do
                              displayBoard a
                              putStrLn "Choose Column:"
                              column <- getInput a player
                              print column

getInput a player = if player == 1
                      then getLine
                      else return $ show $ getChoice a

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
                                                            then gameFunc (fromJust nBoard) (nextChance player)
                                                            else do
                                                              putStr "Player "
                                                              putStr $ show $ fromJust terminal
                                                              putStr " Wins\n"


getChoice board = 1


nextChance 1 = 2
nextChance _ = 1


