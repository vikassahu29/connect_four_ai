module Main where

import Data.Maybe
import Data.List

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

initBoard = take 7 $ repeat $ take 6 $repeat 0

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
                      else return $ getChoice a

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


getChoice board = show 1

displayBoard (x:xs) = do
                       print x
                       displayBoard xs
displayBoard [] = putStrLn "\n"

nextChance 1 = 2
nextChance _ = 1

isTerminal board = find (\a -> a /= 0) [checkForWin board x y xLen yLen | x <- [0.. (xLen - 1)] , y <- [0..(yLen - 1)]]
                    where xLen = length board
                          yLen = length $ board !! 0

checkForWin board x y xLen yLen = if ver /= 0
                                    then ver
                                    else if horz /= 0
                                          then horz
                                          else if diag /= 0
                                                then diag
                                                else 0
                                  where ver = checkWinVer board x y xLen yLen
                                        horz = checkWinHorz board x y xLen yLen
                                        diag = checkWinDiag board x y xLen yLen

                                        
checkWinHorz board x y xLen yLen = if (x + 3) >= xLen
                                      then 0
                                      else if checkEq x01 x02 x03 xy
                                              then xy
                                              else 0
                                        where x01 = getMatrixElem board (x + 1) y
                                              x02 = getMatrixElem board (x + 2) y
                                              x03 = getMatrixElem board (x + 3) y
                                              xy = getMatrixElem board x y

checkWinVer board x y xLen yLen = if (y + 3) >= yLen
                                      then 0
                                      else if checkEq y01 y02 y03 xy
                                              then xy
                                              else 0
                                        where y01 = getMatrixElem board x (y + 1)
                                              y02 = getMatrixElem board x (y + 2)
                                              y03 = getMatrixElem board x (y + 3)
                                              xy = getMatrixElem board x y

checkWinDiag board x y xLen yLen = if (x + 3) >= xLen || (y + 3) >= yLen
                                      then 0
                                      else if checkEq xy1 xy2 xy3 xy
                                              then xy
                                              else 0
                                        where xy1 = getMatrixElem board (x + 1) (y + 1)
                                              xy2 = getMatrixElem board (x + 2) (y + 2)
                                              xy3 = getMatrixElem board (x + 3) (y + 3)
                                              xy = getMatrixElem board x y 



checkEq :: Eq a => a -> a -> a -> a -> Bool
checkEq a1 a2 a3 a = a1 == a && a2 == a && a3 == a

getMatrixElem :: [[a]] -> Int -> Int -> a
getMatrixElem a x y = (a !! x) !! y

makeBoardMove board column player = let pos = getEmptyPosition (board !! column) 0
                                      in if pos == Nothing
                                          then Nothing
                                          else Just $ markBoard board column (fromJust pos) player


getEmptyPosition :: (Num t, Num a, Eq a) => [a] -> t -> Maybe t
getEmptyPosition (x:xs) pos = if x == 0
                                then Just pos
                                else getEmptyPosition xs $ pos + 1
getEmptyPosition [] pos = Nothing


markBoard :: [[a]] -> Int -> Int -> a -> [[a]]
markBoard board column pos player = let (xs, y:ys) = splitAt column board
                                      in xs ++ [markColumn y pos player] ++ ys


markColumn :: [a] -> Int -> a -> [a]
markColumn list pos player = let (xs,_:ys) = splitAt pos list
                                in xs ++ [player] ++ ys
