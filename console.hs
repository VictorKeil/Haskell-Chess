{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification #-}
module Console where

import qualified Data.HashMap as HMap
import Data.List
import Data.List.Index
import qualified System.Console.ANSI as ANSI

import Utils
import Chess

data ConsoleErrorCritical = ConsoleErrorCritical deriving (Show)

instance CriticalError ConsoleErrorCritical where
  trigger = show

data ConsoleError = CancelError Game

instance MoveError ConsoleError ConsoleErrorCritical where
  handleError (CancelError game) = Right game

instance Show Board where
  show brd = showBoard $ showPieces brd

data ConsolePlayer = ConsoleP Color

instance Player ConsolePlayer ConsoleError ConsoleErrorCritical where
  color (ConsoleP colr) = colr
  getMove p = getMoveConsole

-- data ConTest = CT

-- instance CriticalError ConTest where
--   trigger _ = "test"

-- instance Player ConsolePlayer ConsoleError ConTest where
--   color (ConsoleP colr) = colr
--   getMove p = getMoveConsole

modBoard :: Char -> Square -> [String] -> [String]
modBoard c sqrs strs = reverse . imap (rowMapper c sqrs) $ reverse strs
  where rowMapper c sqrs row str = imap (colMapper c sqrs row) str
          where colMapper newC sqrs row col oldC =
                  if (Square (col + 1) (row + 1)) == sqrs
                  then newC else oldC

showPieces :: Board -> [String]
showPieces (Board brd bnds) =
  let textSquares =
        [[if odd (x+y) then ' ' else (toEnum 9632) | x <- [1 .. width bnds]] | y <- [1 .. height bnds]]
  in foldr (\p acc -> modBoard (pieceSym . snd $ p) (fst p) acc) textSquares $ HMap.toList brd
  

markSquaresWith :: Char -> [Square] -> [String] -> [String]
markSquaresWith mark sqrs brd = foldr (modBoard mark) brd sqrs

markSquares :: [Square] -> [String] -> [String]
markSquares sqrs = markSquaresWith 'X' sqrs

showBoard :: [String] -> String
showBoard brd =
  let cols = [toEnum (x + 64) :: Char | x <- [1 .. length . head $ brd]]
      rows = [head . show $ x | x <- [length brd, length brd - 1 .. 1]]
      withAxes = (transpose $ rows:(replicate 8 ' '):(transpose brd))
  in unlines $ (map (intersperse '|') withAxes) ++ [(replicate 10 ' '), "    " ++ intersperse '|' cols]

showBoardFlipped :: [String] -> String
showBoardFlipped brd =
  let cols = reverse [toEnum (x + 64) :: Char | x <- [1 .. length . head $ brd]]
      rows = [head . show $ x | x <- [1 .. length brd]]
      flipped = reverse . map reverse $ brd
      withAxes = (transpose $ rows:(replicate 8 ' '):(transpose flipped))
  in unlines $ (map (intersperse '|') withAxes) ++ [(replicate 10 ' '), "    " ++ intersperse '|' cols]

printBoard :: String -> IO ()
printBoard brd = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0
  putStrLn $ brd

printGame :: Game -> IO ()
printGame (Game brd _ (WPlayer p:_)) = do
  printBoard $ (if (color p) == White then showBoard else showBoardFlipped) $ showPieces brd
  putStrLn $ show (color p) ++ " to move"

printSquares :: Game -> [Square] -> IO ()
printSquares (Game brd _ (WPlayer p:_)) sqrs = do
  printBoard $ (if (color p) == White then showBoard else showBoardFlipped)
    $ markSquaresWith '.' sqrs $ showPieces brd

printMoves :: Game -> [Square] -> IO ()
printMoves game (s:[]) = printSquares game [s]
printMoves (Game brd _ (WPlayer p:_)) (s:ss) =
  printBoard $ (if (color p) == White then showBoard else showBoardFlipped)
    $ markSquaresWith 'O' [s] $ markSquaresWith '.' ss $ showPieces brd

safeSquare :: [Square] -> String -> Maybe Square
safeSquare bnds [a,n] = let sqr = strSqr [a,n]
                        in if elem sqr bnds then Just sqr else Nothing
safeSquare _ _ = Nothing

squareSelect :: String -> [Square] -> IO (Maybe Square)
squareSelect msg bnds = do
  putStr msg
  sqr <- getLine
  case sqr of
    "cancel" -> return Nothing
    sqr -> case safeSquare bnds sqr of
             Nothing -> do putStrLn "Not a valid square, try again."
                           squareSelect msg bnds
             Just sqr -> return $ Just sqr

freeSelect :: Game -> IO (Either ConsoleError Move)
freeSelect game = do
  putStr "Enter any move: "
  str <- getLine
  case str of
    "cancel" -> return $ Left (CancelError game)
    [a,n,' ',a',n'] -> return $ Right (strSqr [a,n], strSqr [a',n'])
    _ -> do
      putStrLn "Invalid format, try again."
      freeSelect game

getMoveConsole :: Game -> IO (Either ConsoleError Move)
getMoveConsole game@(Game brd lastMv ps@(WPlayer p:_)) = do
  printGame game
  sqr1 <- squareSelect "Enter square to move: " $ coloredSquares brd (color p)
  case sqr1 of
    Nothing -> return $ Left (CancelError game)
    Just s1 -> do
      let vm = validMoves brd $ lookupS' brd s1
      printMoves (Game brd lastMv ps) (s1:vm)
      sqr2 <- squareSelect "Enter destination square: " vm
      case sqr2 of
        Nothing -> return $ Left (CancelError game)
        Just s2 -> do
          printGame (Game (handleMove brd s1 s2) lastMv ps)
          return $ Right (s1, s2)
  where notCheck sqr1 sqr2 = null . checks (handleMove brd sqr1 sqr2) $ (color p)

getMoveConsoleDebug game = do
  printGame game
  freeSelect game
