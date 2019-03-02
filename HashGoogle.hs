module Main where
import Data.List.Split
import Data.Char (isDigit)
import qualified Data.Set as Set
import System.Random
import System.IO.Unsafe
import System.IO

type Slideshow = [Pic]
type Pic = (Int, Ori, Set.Set String)
type Tag = String
type Ori = String

main = do
    contents1 <- readFile "C:\\Users\\Lorenzo\\Desktop\\127_001\\PHYTON\\GHimp1.txt"
    contents2 <- readFile "C:\\Users\\Lorenzo\\Desktop\\127_001\\PHYTON\\GHimp2.txt"
    contents3 <- readFile "C:\\Users\\Lorenzo\\Desktop\\127_001\\PHYTON\\GHimp3.txt"
    contents4 <- readFile "C:\\Users\\Lorenzo\\Desktop\\127_001\\PHYTON\\GHimp4.txt"
    --contents5 <- readFile "C:\\Users\\Lorenzo\\Desktop\\127_001\\PHYTON\\GHimp5.txt"
    writeFile "GHout1.txt" (printer . parse $ contents1)
    writeFile "GHout2.txt" (printer . parse $ contents2)
    writeFile "GHout3.txt" (printer . parse $ contents3)
    writeFile "GHout4.txt" (printer . parse $ contents4)
    writeFile "GHout5.txt" (doE)
    print . execA . parse $ contents1
    print . execA . parse $ contents2
    print . execA . parse $ contents3
    print . execA . parse $ contents4
    --print . execE . parse $ contents5

doE :: String
doE = unlines . ("40000":) . map (\x -> show x ++ " " ++ show (x+1)) $ [0,2..79998]

--parse by line based on '\n' Char
parse :: String -> Slideshow
--parse str = zip [0..] (map words . tail . lines $ str)
--parse str = [(id, or, xs) | (id,[or:_:xs]) <- zip [0..] (map words . tail . lines $ str)]
parse str = [(id, "H", makeSet xs) | (id,("H":_:xs)) <- zip [0..] (map words . tail . lines $ str)]

printer :: Slideshow -> String
printer ls = unlines $ (show (length ls)) : map (\(id,_,_) -> show id) ls

makeSet :: [String] -> Set.Set String
makeSet = Set.fromList

execA :: Slideshow -> Int
execA = findScore -- . unsafePerformIO . shuffleList

findScore :: Slideshow -> Int
findScore [_] = 0
findScore (x:y:zs) = cuple x y + findScore (y:zs)

cuple :: Pic -> Pic -> Int
cuple (_,_,t1) (_,_,t2) = minimum . map Set.size $ [inte, Set.difference t1 inte, Set.difference t2 inte]
  where
    inte = Set.intersection t1 t2

shuffleList :: [a] -> IO [a]
shuffleList [] = return [] -- An empty list cannot be shuffled more
shuffleList [x] = return [x] -- A list with one element cannot be shuffled more
shuffleList as = do
    i <- randomRIO (0,length as-1)
    shuffledRest <- shuffleList (take i as ++ drop (i+1) as)
    return $ (as !! i) : shuffledRest