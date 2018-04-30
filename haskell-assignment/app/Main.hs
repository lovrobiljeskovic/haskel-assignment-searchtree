module Main where

import Lib
import System.IO
import Data.IORef

main :: IO ()
main = do
  treeRef <- newIORef BinaryEmpty
  writeIORef treeRef BinaryEmpty
  tree <- readIORef treeRef
  handleCommand tree

data BinaryTree a = BinaryEmpty
                  | BinaryNode a (BinaryTree a) (BinaryTree a)
                  deriving (Show)


handleCommand :: BinaryTree Int -> IO ()
handleCommand tree = do
  putStrLn "Available options: (1) Search item (2) Insert item (3) list all items in order (4) Load items from file"
  line <- getLine
  let command = (read line :: Int)
  case command of
    1 -> do
      result <- handleSearch tree
      putStrLn (show result)
      handleCommand tree
    2 -> do
      newTree <- handleInsert tree
      putStrLn (show (inOrder newTree))
      handleCommand newTree
    3 -> do
      putStrLn (show (inOrder tree))
      handleCommand tree
    4 -> do
      newTree <- handleNumbersUpload
      handleCommand newTree
    _ -> do
      putStrLn "please provide a valid number"
      handleCommand tree


handleSearch :: BinaryTree Int -> IO (Bool)
handleSearch tree = do
  putStrLn "enter a number to search "
  line <- getLine
  let number = (read line :: Int)
  return (searchBinaryTree number tree)

handleInsert :: BinaryTree Int -> IO (BinaryTree Int)
handleInsert tree = do
  putStrLn "enter a number to insert "
  line <- getLine
  let number = (read line :: Int)
  return (insert number tree)

handleNumbersUpload :: IO (BinaryTree Int)
handleNumbersUpload = do
  putStrLn "what is the pathName of the file"
  line <- getLine
  let file = (read line :: String)
  newTree <- fromIntegerFile file
  return (newTree)

searchBinaryTree :: (Ord a) => a -> BinaryTree a -> Bool
searchBinaryTree  _ BinaryEmpty  =  False
searchBinaryTree x (BinaryNode value binaryTreeLeft binaryTreeRight)
            | x == value =  True
            | x < value = searchBinaryTree x binaryTreeLeft
            | otherwise = searchBinaryTree x binaryTreeRight


insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert newValue BinaryEmpty = BinaryNode newValue BinaryEmpty BinaryEmpty
insert newValue (BinaryNode value2 binaryTreeLeft binaryTreeRight)
            | newValue < value2 = BinaryNode value2 (insert newValue binaryTreeLeft) binaryTreeRight
            | otherwise = BinaryNode value2 binaryTreeLeft (insert newValue binaryTreeRight)

inOrder :: (Ord a) => BinaryTree a -> [a]
inOrder BinaryEmpty = []
inOrder (BinaryNode v binaryTreeLeft  binaryTreeRight) = inOrder binaryTreeLeft ++ [v] ++ inOrder binaryTreeRight

fromIntegerFile :: String -> IO (BinaryTree Int)
fromIntegerFile text = do
  withFile text ReadMode (\handle -> do
    contents <- hGetContents handle
    let contentLines = lines contents
    let values = map read contentLines :: [Int]
    let tree = foldr insert BinaryEmpty values
    putStrLn (show $ inOrder tree)
    return tree
)
