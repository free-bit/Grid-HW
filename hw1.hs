module Hw1 (next) where

uld x y lst
    | x >= 1 && y >= 1 && lst!!(y-1)!!(x-1) == "B" = 1
    | otherwise = 0

up x y lst
    | y >= 1 && lst!!(y-1)!!x == "B" = 1
    | otherwise = 0

urd x y wid lst
    | x+1 < wid && y >= 1 && lst!!(y-1)!!(x+1) == "B" = 1
    | otherwise = 0

left x y lst
    | x >= 1 && lst!!y!!(x-1) == "B" = 1
    | otherwise = 0

right x y wid lst
    | x+1 < wid && lst!!y!!(x+1) == "B" = 1
    | otherwise = 0

dld x y len lst
    | x >= 1 && y+1 < len && lst!!(y+1)!!(x-1) == "B" = 1
    | otherwise = 0

down x y len lst 
    | y+1 < len && lst!!(y+1)!!x == "B" = 1
    | otherwise = 0

drd x y wid len lst
    | x+1 < wid && y+1 < len && lst!!(y+1)!!(x+1) == "B" = 1
    | otherwise = 0

count x y wid len lst = (uld x y lst) + (up x y lst) + (urd x y wid lst) + (left x y lst) + (right x y wid lst) + (dld x y len lst) + (down x y len lst) + (drd x y wid len lst)

check x y lst
    | lst!!y!!x == "B" && (counted == 2 || counted == 3) = "B"
    | lst!!y!!x == "B" && (counted < 2 || counted > 3) = "W"
    | lst!!y!!x == "W" && counted == 3 = "B"
    | otherwise = lst!!y!!x
    where
        counted = count x y wid len lst
        wid = length (head lst)
        len = length lst

construct lst = [check x y lst | (x,y) <- grid]
                where 
                    grid = [(x,y) | y <- [0..((length lst)-1)], x <- [0..((length (head lst))-1)]]

parse xs wid
  | null xs   = []
  | otherwise = (take wid xs):(parse (drop wid xs) wid)

--Main Function
next :: [[String]] -> [[String]]
next []  = []
next lst = parse nlst wid
            where
                nlst = construct lst
                wid  = length (head lst)

createFile fileName = writeFile fileName ""

writeSteps filename grid n = do
                    createFile filename
                    writeStepsHelper filename grid n n
    where writeStepsHelper filename grid n all = if n == 0 then putStrLn ((show all) ++ " steps are succesfully written to \'" ++ filename ++ "\'") else do
                                    appendFile filename ((show (next grid)) ++ "\n")
                                    writeStepsHelper filename (next grid) (n-1) all