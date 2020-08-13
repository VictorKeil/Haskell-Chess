module Console where

import qualified Data.HashMap as HMap
import Data.List
import Data.List.Index
import qualified System.Console.ANSI as ANSI

import Utils
import Chess

instance Show Board where
  show brd = showBoard $ showPieces brd

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
printGame (Game brd _ (P _ trn:_)) = do
  printBoard $ (if trn == White then showBoard else showBoardFlipped) $ showPieces brd
  putStrLn $ show trn ++ " to move"

printSquares :: Game -> [Square] -> IO ()
printSquares (Game brd _ (P _ trn:_)) sqrs = do
  printBoard $ (if trn == White then showBoard else showBoardFlipped)
    $ markSquaresWith '.' sqrs $ showPieces brd

printMoves :: Game -> [Square] -> IO ()
printMoves game (s:[]) = printSquares game [s]
printMoves (Game brd _ (P _ trn:_)) (s:ss) =
  printBoard $ (if trn == White then showBoard else showBoardFlipped)
    $ markSquaresWith 'O' [s] $ markSquaresWith '.' ss $ showPieces brd

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

getMove :: Game -> IO (Maybe Move)
getMove game@(Game brd lastMv ps@(P _ trn:_)) = do
  printGame game
  sqr1 <- squareSelect "Enter square to move: " $ coloredSquares brd trn
  case sqr1 of
    Nothing -> return Nothing
    Just s1 -> do
      let vm = validMoves brd $ lookupS' brd s1
      printMoves (Game brd lastMv ps) (s1:vm)
      sqr2 <- squareSelect "Enter destination square: " vm
      case sqr2 of
        Nothing -> return Nothing
        Just s2 -> return $ Just (s1, s2)
  where notCheck sqr1 sqr2 = null . checks (handleMove brd sqr1 sqr2) $ trn
