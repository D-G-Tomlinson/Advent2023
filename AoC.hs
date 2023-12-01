import Data.Char
import System.IO

day1Part1 :: IO ()
day1Part1 = do
  answer <- dod1p1
  print answer
  where
    dod1p1 :: IO Int
    dod1p1 = do
      file <- readFile "textFiles/day1"
      return (sum (map findNum (lines file)))
        where
          findNum :: String -> Int
          findNum (c:s)
                  | isDigit c = 10 * digitval c + findLastDigitNum (digitval c) s
                  | otherwise = findNum s
            where
              findLastDigitNum :: Int -> String -> Int
              findLastDigitNum result [] = result
              findLastDigitNum result (c:s)
                      | isDigit c = findLastDigitNum (digitval c)  s
                      | otherwise = findLastDigitNum result s
              digitval :: Char -> Int
              digitval n = ord n - 48

day1Part2 :: IO ()
day1Part2 = do
  answer <- dod1p2
  print answer
  where
    dod1p2 :: IO Int
    dod1p2 = do
      file <- readFile "textFiles/day1"
      return (sum (map findNum (lines file)))
        where
          findNum :: String -> Int
          findNum (c:s)
                  | val /= -1 = 10 * val + findLastDigitNum val s
                  | otherwise = findNum s
            where
              val = digitVal (c:s)
              digitVal :: String -> Int
              digitVal ('o':'n':'e':_)         = 1
              digitVal ('t':'w':'o':_)         = 2
              digitVal ('t':'h':'r':'e':'e':_) = 3
              digitVal ('f':'o':'u':'r':_)     = 4
              digitVal ('f':'i':'v':'e':_)     = 5
              digitVal ('s':'i':'x':_)         = 6
              digitVal ('s':'e':'v':'e':'n':_) = 7
              digitVal ('e':'i':'g':'h':'t':_) = 8
              digitVal ('n':'i':'n':'e':_)     = 9
              digitVal (c:s)
                      | isDigit c              = ord c - 48
                      | otherwise              = -1
              findLastDigitNum :: Int -> String -> Int
              findLastDigitNum result []     = result
              findLastDigitNum result (c:s)
                      | digitVal (c:s) /= -1 = findLastDigitNum (digitVal (c:s)) s
                      | otherwise            = findLastDigitNum result s
