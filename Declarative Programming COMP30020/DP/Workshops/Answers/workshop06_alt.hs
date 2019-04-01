-- Alternative solution to Workshop 6 Question 5
-- Author:  Rob Holt

import Data.List
import Data.Char
import Control.Monad
import System.IO

{-
 ==== Data Structute Definitions ====
  * A Binary Search Tree (BST) of key-value pairs
  * A Foldable instance of the tree, which buys us:
    * foldr
    * mapM_
  * Data structure interface functions to:
    * Convert the BST to and from a list
    * Add, remove and lookup items in the tree
-}

data Tree a = Nil
              | Node a (Tree a) (Tree a)

instance Foldable Tree where
  foldr f acc Nil = acc
  foldr f acc (Node keyVal l r) = foldr f (f keyVal (foldr f acc r)) l

-- Convert a tree to a list
toList :: Tree a -> [a]
toList t = foldr (:) [] t

-- Convert a list of key-value pairs to a BST
fromList :: Ord k => [(k, v)] -> Tree (k, v)
fromList lst = foldr add Nil $ sortOn fst lst

-- Add a new key-value pair to a BST
add :: Ord k => (k, v) -> Tree (k, v) -> Tree (k, v)
add (key, val) Nil = Node (key, val) Nil Nil
add (key, val) (Node (kt, vt) l r) 
  | key <= kt = Node (kt, vt) (add (key, val) l) r
  | otherwise = Node (kt, vt) l (add (key, val) r)

-- Remove a key from a BST
-- Note that we take the opportunity to ameliorate the
-- tree's structure when forced to update it
remove :: Ord k => k -> Tree (k, v) -> Tree (k, v)
remove _ Nil = Nil
remove key (Node (kt, vt) l r) 
  | key == kt = rebalance l r
  | key <= kt = Node (kt, vt) (remove key l) r
  | otherwise = Node (kt, vt) l (remove key r)

-- Construct a balanced BST from two BSTs
rebalance :: Ord k => Tree (k, v) -> Tree (k, v) -> Tree (k, v)
rebalance l r = buildBalanced $ merge lList rList
  where
    lList = toList l
    rList = toList r

-- Merge two sorted lists of key-value pairs
merge :: Ord k => [(k, v)] -> [(k, v)] -> [(k, v)]
merge [] lst = lst
merge lst [] = lst
merge ((kx, vx):xs) ((ky, vy):ys)
  | kx <= ky  = (kx, vx) : merge xs ((ky, vy):ys)
  | otherwise = (ky, vy) : merge ((kx, vx):xs) ys

-- Split a list into two lists, each with half the elements
demerge :: [a] -> ([a], [a])
demerge l = go ([], []) l
  where
    go (l1, l2) (x:y:xs) = go (x:l1, y:l2) xs
    go (l1, l2) [x]      = (x:l1, l2)
    go acc      _        = acc

-- Build a balanced key-value tree from a list
buildBalanced :: Ord k => [(k, v)] -> Tree (k, v)
buildBalanced [] = Nil
buildBalanced l  =
  let (l1, l2) = demerge l in
  case uncons l1 of
    Just ((key, val), l1') ->
      Node (key, val) (buildBalanced l1') (buildBalanced l2)
    Nothing -> buildBalanced l2

-- Get all values with a given key from the BST
getValues :: Ord k => k -> Tree (k, v) -> [v]
getValues key tree = go [] key tree
  where
    go acc key Nil = acc
    go acc key (Node (kt, vt) l r)
      | key == kt = go (vt:acc) key l
      | key <  kt = go acc key l
      | otherwise = go acc key r

{-
 ==== Phone Directory Interface Definitions ====
   * Datatypes
   * Directory API
-}

type Name = String

type PhoneNumber = String

type PhoneBook = Tree (Name, PhoneEntry)

data PhoneEntry = PhoneEntry
  { phoneNumber  :: PhoneNumber
  }
  deriving Show

data Command = Print
             | Add Name PhoneNumber
             | Delete Name
             | Lookup Name
             | Quit
             deriving (Eq, Show)

-- Add an entry into a phonebook
addEntry :: Name -> PhoneNumber -> PhoneBook -> PhoneBook
addEntry name number phoneBook = add (name, entry) phoneBook
  where
    entry = PhoneEntry { phoneNumber = number }

-- Remove an entry from a phonebook
removeEntries :: Name -> PhoneBook -> PhoneBook
removeEntries name phoneBook = remove name phoneBook

-- Lookup all entries matching a name in a phonebook
lookupEntries :: Name -> PhoneBook -> [PhoneEntry]
lookupEntries name phoneBook = getValues name phoneBook

-- Print a single phonebook entry
printEntry :: (Name, PhoneEntry) -> IO ()
printEntry (name, phoneEntry) = putStrLn $ name ++ ": " ++ phoneNumber phoneEntry

-- Print all entries in a given phonebook
printEntries :: PhoneBook -> IO ()
printEntries phoneBook = mapM_ printEntry phoneBook

-- Print results returned from a phonebook lookup
printLookupResult :: Name -> PhoneBook -> IO ()
printLookupResult name phoneBook = case lookupEntries name phoneBook of
  []      ->  putStrLn "No entry found"
  entries ->  mapM_ (\entry -> printEntry (name, entry)) entries

-- Interpret a phonebook interface command from a string
readCommand :: String -> Maybe Command
readCommand s = readCommand' $ words s
  where
    readCommand' [w]
      | isCommand "quit" w    = Just Quit
      | isCommand "print" w   = Just Print
    readCommand' [w1, w2]
      | isCommand "delete" w1 = Just $ Delete w2
      | isCommand "lookup" w1 = Just $ Lookup w2
    readCommand' [w1, w2, w3]
      | isCommand "add" w1    = Just $ Add w2 w3
    readCommand' _            = Nothing

    isCommand command word = map toLower word `elem` [take 1 command, command]

-- Prompt the terminal with a message
prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

-- Prompt the terminal and return a possible command
promptCommand :: String -> IO (Maybe Command)
promptCommand msg = do
  s <- prompt msg
  return $ readCommand s

-- Main REPL function
mainLoop :: PhoneBook -> IO ()
mainLoop phoneBook = do
  command <- promptCommand "> "
  case command of
    Just Quit  -> return ()
    Just Print -> do
      printEntries phoneBook
      mainLoop phoneBook
    Just (Lookup name) -> do
      printLookupResult name phoneBook
      mainLoop phoneBook
    Just (Add name number) -> mainLoop $ addEntry name number phoneBook
    Just (Delete name) -> mainLoop $ removeEntries name phoneBook
    Nothing -> do
      putStrLn "Invalid command"
      mainLoop phoneBook

main :: IO ()
main = do
  putStrLn "==== Welcome to HaskellDirectory 3000 ====\n"
  mainLoop $ fromList []
  putStrLn "\n==== Thank you for using HaskellDirectory 3000 ====\n"
