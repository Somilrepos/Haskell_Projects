module Main where

import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Data.Text (pack, unpack, strip)

calculator :: Double -> Char -> Double -> Double
calculator x op y = case op of
  '+' -> x + y
  '-' -> x - y
  '*' -> x * y
  '/' -> x / y
  _   -> error "Invalid operator"

readDouble :: String -> IO Double
readDouble prompt = do
  putStr prompt >> hFlush stdout
  line <- getLine
  case readMaybe line of
    Just n  -> pure n
    Nothing -> putStrLn "Invalid number, try again." >> readDouble prompt

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

readOp :: String -> IO Char
readOp prompt = do
  putStr prompt >> hFlush stdout

  line <-  trim <$> getLine
  case line of
    (c:_) | c `elem` "+-*/" -> pure c
    _ -> putStrLn "Invalid operator, try again." >> readOp prompt

main :: IO ()
main = do
  putStrLn "Simple calculator"
  x  <- readDouble "Enter first number: "
  op <- readOp     "Enter operator (+ - * /): "
  y  <- readDouble "Enter second number: "
  if op == '/' && y == 0
    then putStrLn "Error: division by zero"
    else print (calculator x op y)

