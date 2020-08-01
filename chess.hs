module Chess where

import Data.Maybe
import Data.List.Index
import qualified System.Console.ANSI as ANSI
import Data.Char
import Data.Tuple
import Data.List
import qualified Data.HashMap as HMap
import Data.Hashable

data Square = Square Int Int deriving (Eq, Ord)

instance Show Square where
  show (Square col row) = toEnum (col + 64) : show row

instance Hashable Square where
  hash (Square col row) = col + row * 8
  hashWithSalt _ sqr = hash sqr

data Color = Black | White deriving (Show, Eq, Enum, Bounded)

data PieceType = King | Queen | Pawn | Rook | Bishop | Knight deriving (Eq, Show, Enum, Bounded, Read)

data Piece = Piece PieceType Color Square Square deriving (Show)

data Bounds = Bounds {minX :: Int, minY :: Int, maxX :: Int, maxY :: Int} deriving (Show)
width :: Bounds -> Int
width (Bounds minX _ maxX _) = maxX - minX + 1

height :: Bounds -> Int
height (Bounds _ minY _ maxY) = maxY - minY + 1

square :: Piece -> Square
square (Piece _ _ sqr _) = sqr

pieceSym :: Piece -> Char
pieceSym (Piece Knight _ _ _) = 'N'
pieceSym (Piece kind _ _ _) = head $ show kind

type BoardMap = HMap.Map Square Piece

data Board = Board BoardMap Bounds
  
lookupS :: Board -> Square -> Maybe Piece
lookupS (Board brd _) sqr = HMap.lookup sqr brd

modBoard :: Char -> Square -> [String] -> [String]
modBoard c sqrs strs = reverse . imap (rowMapper c sqrs) $ reverse strs
  where rowMapper c sqrs row str = imap (colMapper c sqrs row) str
          where colMapper newC sqrs row col oldC =
                  if (Square (col + 1) (row + 1)) == sqrs
                  then newC else oldC

showPieces :: Board -> [String]
showPieces (Board brd bnds) =
  let textSquares =
        [[if odd (x+y) then ' ' else '#' | x <- [1 .. width bnds]] | y <- [1 .. height bnds]]
  in foldr (\p acc -> modBoard (pieceSym . snd $ p) (fst p) acc) textSquares $ HMap.toList brd
  

markSquaresWith :: Board -> Char -> [Square] -> [String]
markSquaresWith (Board brd bnds) mark sqrs = foldr (modBoard mark) (showPieces (Board brd bnds)) sqrs

markSquares :: Board -> [Square] -> [String]
markSquares brd = markSquaresWith brd 'X'

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

instance Show Board where
  show brd = showBoard $ showPieces brd

printBoard :: [String] -> IO ()
printBoard brd = putStrLn $ showBoard brd

sameColor :: BoardMap -> Color -> Square -> Bool
sameColor brd col1 sqr = case HMap.lookup sqr brd of
                          Nothing -> False
                          Just (Piece _ col2 _ _) -> col1 == col2

nullSquare :: (HMap.Map Square Piece) -> Square -> Bool
nullSquare brd = null . (flip HMap.lookup $ brd)

-- Like takeWhile, but includes the first element for which the predicate is false
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x:xs) = if f x then x:(takeUntil f xs) else [x]

-- Move mechanics for long range pieces
takeLong :: BoardMap -> [Square] -> [Square]
takeLong brd = takeUntil (nullSquare brd)

restrictToBounds :: Bounds -> [Square] -> [Square]
restrictToBounds (Bounds minX minY maxX maxY) ss = filter inBounds ss
  where inBounds (Square col row) =
          (col `elem` [minX .. maxX]) && (row `elem` [minY .. maxY])

targetSquares :: Board -> Piece -> [Square]
targetSquares (Board brd bnds) (Piece Pawn color (Square col row) _) =
  let (direction, initRow) = if color == White then (1,2) else (-1, 7)
  in restrictToBounds bnds
     $ [Square (col - 1) (row + direction), Square (col + 1) (row + direction)]
  
targetSquares (Board brd bnds) (Piece Rook color (Square col row) _) =
  let posX = [Square (col + x) row | x <- [1..maxX bnds - col]]
      negX = [Square (col - x) row | x <- [1..col - minX bnds]]
      posY = [Square col (row + y) | y <- [1..maxY bnds - row]]
      negY = [Square col (row - y) | y <- [1..row - minY bnds]]
  in concat $ map (takeLong brd) $ [negX, posX, negY, posY]

targetSquares (Board brd bnds) (Piece Bishop color (Square col row) _) =
  let maxNorth = maxY bnds - row
      maxSouth = row - minY bnds
      maxWest = col - minX bnds
      maxEast = maxX bnds - col
      
      ne = [Square (col + x) (row + x) | x <- [1 .. min maxNorth maxEast]]
      se = [Square (col + x) (row - x) | x <- [1 .. min maxSouth maxEast]]
      sw = [Square (col - x) (row - x) | x <- [1 .. min maxSouth maxWest]]
      nw = [Square (col - x) (row + x) | x <- [1 .. min maxNorth maxWest]]
  in concat $ map (takeLong brd) $ [ne, se, sw, nw]

targetSquares (Board brd bnds) (Piece Knight color (Square col row) _) =
  let set1 = [Square (col + x) (row + y) | x <- [-1, 1], y <- [-2, 2]]
      set2 = [Square (col + x) (row + y) | x <- [-2, 2], y <- [-1,1]]
  in restrictToBounds bnds $ set1 ++ set2

targetSquares brd (Piece Queen color sqr lSqr) =
  concat $ map (targetSquares brd) [(Piece Rook color sqr lSqr), (Piece Bishop color sqr lSqr)]

targetSquares (Board brd bnds) (Piece King color (Square col row) _) =
  let set1 = [Square (col + x) (row + y) | x <- [-1..1], y <- [-1,1]]
  in restrictToBounds bnds $ set1 ++ [Square (col + x) row | x <- [-1,1]]

-- Takes a function and its argument, applies the function to the argument and stores
-- the result and the argument in a pair
fArgPair :: (a -> b) -> a -> (b, a)
fArgPair f x = (f x, x)

targeters :: (Board -> Piece -> [Square]) -> Board -> Color -> Square -> [Square]
targeters moveFunc brd color sqr =
  let pieces = [Piece t color sqr (Square 1 1) | t <- [minBound..maxBound::PieceType]]
      -- Use the provided moveFunc to find all the squares that each pieceType targets. If an enemy
      -- piece of the same type occupies a targeted square, it can capture on that square
      pieceTargets :: [([Square], Piece)]
      pieceTargets = map (fArgPair $ moveFunc brd) pieces
  in concat $ map occupiedBy pieceTargets
  where occupiedBy (sqrs, pc) = filter (isEnemyOf pc) sqrs
          where isEnemyOf (Piece t c _ _) sqr' = case lookupS brd sqr' of
                                                   Nothing -> False
                                                   Just (Piece t' c' _ _) -> (t == t') && (c /= c')
      
targeted :: Board -> Color -> Square -> Bool
targeted brd color sqr = not . null $ targeters targetSquares brd color sqr

king :: Board -> Color -> Maybe Piece
king brd colr = find (\(Piece t _ _ _) -> t == King) $ coloredPieces brd colr

checks :: Board -> Color -> [Square]
checks brd colr = 
  case king brd colr of
    Nothing -> []
    Just (Piece _ _ sqr _) -> targeters targetSquares brd colr sqr

check :: Board -> Color -> Bool
check brd colr = not . null $ checks brd colr

move :: Board -> Piece -> Square -> Board
move brd pc@(Piece t c sqr1 _) sqr2 = insertPiece (removePiece brd pc) (Piece t c sqr2 sqr1)

zipFilter :: [Bool] -> [a] -> [a]
zipFilter [] _ = []
zipFilter _ [] = []
zipFilter (b:bs) (x:xs) = if b then x:(zipFilter bs xs) else zipFilter bs xs

validCastles :: Board -> Piece -> [Square]
validCastles brd@(Board _ bnds) k@(Piece King colr (Square col row) _) =
  if moved (Just k)
  then []
  else let left = col - 2
           right = col + 2
           rookSqrs = [Square x row | x <- [minX bnds, maxX bnds]]
           rooksMoved = map (moved . lookupS brd) rookSqrs
           castleSqrs = [[Square x row | x <- [left .. col]]
                           ,[Square x row | x <- [col .. right]]]
           rookValid = zipFilter (map not rooksMoved) castleSqrs
           canCastle = map (not . foldr (\s acc -> acc || targeted brd colr s) False) rookValid
           
       in zipFilter canCastle [Square left row, Square right row]
           
           
  where moved (Just (Piece _ _ sqr lSqr)) = sqr /= lSqr
        moved Nothing = True

validMoves :: Board -> Piece -> [Square]
validMoves (Board brd bnds) pc@(Piece Pawn color (Square col row) lSqr) =
  let (direction, initRow) = if color == White then (1,2) else (-1,7)
      baseSquares =
        if row == initRow
        then [Square col (row + direction * x) | x <- [1..2]]
        else [Square col (row + direction)]
      baseMoves = takeWhile (nullSquare brd) baseSquares
      
      tgtSqrs = targetSquares (Board brd bnds) (Piece Pawn color (Square col row) lSqr)
      validCaptures = filter (\s -> not (sameColor brd color s || nullSquare brd s)) tgtSqrs
      preCheck = restrictToBounds bnds $ validCaptures ++ baseMoves
  in filter (\s -> not $ check (move (Board brd bnds) pc s) color) preCheck

validMoves (Board brd bnds) (Piece King color sqr lSqr) =
  let tgtSqrs = targetSquares (Board brd bnds) (Piece King color sqr lSqr)
      castles = validCastles (Board brd bnds) (Piece King color sqr lSqr)
      diffColor = filter (not . sameColor brd color) $ tgtSqrs
  in castles ++ filter (not . targeted (Board brd bnds) color) diffColor
  
validMoves brd@(Board brdMap bnds) pc@(Piece ptype colr sqr lSqr) =
  let preCheck = filter (not . sameColor brdMap colr) $ targetSquares brd pc
  in filter (\s -> not $ check (move brd pc s) colr) preCheck

coloredPieces :: Board -> Color -> [Piece]
coloredPieces (Board brd _) color =
  map snd $ filter ((\(Piece _ color' _ _) -> color' == color) . snd) $ HMap.toList brd

coloredSquares :: Board -> Color -> [Square]
coloredSquares brd color = map (\(Piece _ _ sqr _) -> sqr) $ coloredPieces brd color

checkMate :: Board -> Color -> Bool
checkMate brd colr =
  case king brd colr of
    Nothing -> False
    Just k ->
      let kCanMove = not . null $ validMoves brd k
          checks' = checks brd colr
          checkAttackers = if null checks' then []
                           else case lookupS brd (head checks') of
                                  Nothing -> []
                                  Just (Piece _ color' _ _) -> map (targeters validMoves brd color') checks'
          canKillCheck = 1 == (length . filter (not . null) $ checkAttackers)
      in not . or $ [null checks', kCanMove, canKillCheck]

lookupS' :: Board -> Square -> Piece
lookupS' brd sqr = case lookupS brd sqr of
                     Nothing -> error ("The square '" ++ show sqr ++ "' does not contain a piece.")
                     Just sqr -> sqr

strSqr :: String -> Square
strSqr (a:n:rs) = Square (fromEnum (toUpper a) - 64) (fromEnum n - 48)

emptyBoard :: Board
emptyBoard = Board HMap.empty (Bounds 1 1 8 8)

moves :: Board -> Square -> [Square]
moves brd sqr = case lookupS brd sqr of Nothing -> []
                                        Just p -> validMoves brd p

insertPiece :: Board -> Piece -> Board
insertPiece (Board brd bnds) pc@(Piece t c sqr _) =
  (Board (HMap.insert sqr pc brd) bnds)

removePiece :: Board -> Piece -> Board
removePiece (Board brd bnds) (Piece _ _ sqr _) =
  (Board (HMap.delete sqr brd) bnds)

board = foldr (flip insertPiece) emptyBoard pieces
  where pieces = let whitePRow = 2
                     blackPRow = 7
                     whitePawns = [Piece Pawn White (Square col whitePRow) (Square col whitePRow) | col <- [1..8]]
                     blackPawns = [Piece Pawn Black (Square col blackPRow) (Square col blackPRow) | col <- [1..8]]
                     whiteRooks = [Piece Rook White (Square col 1) (Square col 1) | col <- [1,8]]
                     blackRooks = [Piece Rook Black (Square col 8) (Square col 8) | col <- [1,8]]
                     whiteKnights = [Piece Knight White (Square col 1) (Square col 1) | col <- [2,7]]
                     blackKnights = [Piece Knight Black (Square col 8) (Square col 8) | col <- [2,7]]
                     whiteBishops = [Piece Bishop White (Square col 1) (Square col 1) | col <- [3, 6]]
                     blackBishops = [Piece Bishop Black (Square col 8) (Square col 8) | col <- [3, 6]]
                     whiteQueen = [Piece Queen White (Square 4 1) (Square 4 1)]
                     blackQueen = [Piece Queen Black (Square 4 8) (Square 4 8)]
                     whiteKing = [Piece King White (Square 5 1) (Square 5 1)]
                     blackKing = [Piece King Black (Square 5 8) (Square 5 8)]
          in (whitePawns ++ whiteRooks ++ whiteKnights ++ whiteBishops ++ whiteQueen ++ whiteKing)
          ++ (blackPawns ++ blackRooks ++ blackKnights ++ blackBishops ++ blackQueen ++ blackKing)

safeSquare :: [Square] -> String -> Maybe Square
safeSquare bnds [a,n] = let sqr = strSqr [a,n]
                        in if elem sqr bnds then Just sqr else Nothing
safeSquare _ _ = Nothing

cycleEnumSucc :: (Eq a, Enum a, Bounded a) => a -> a
cycleEnumSucc e = if e == maxBound then minBound else succ e

cycleEnumPred :: (Eq a, Enum a, Bounded a) => a -> a
cycleEnumPred e = if e == minBound then maxBound else pred e

data Game = Game Board Color
instance Show Game where
  show (Game _ trn) = "Game - Turn: " ++ show trn

printGame :: Game -> IO ()
printGame (Game brd trn) = do
  putStrLn $ showBoardFlipped $ showPieces brd
  putStrLn $ show trn ++ " to move"

pawnPromote :: Board -> Square -> Maybe Color
pawnPromote brd@(Board _ bnds) sqr = case lookupS' brd sqr of
                        (Piece Pawn White (Square _ row) _) ->
                          if row == maxY bnds then Just White else Nothing
                        (Piece Pawn Black (Square _ row) _) ->
                          if row == minY bnds then Just Black else Nothing
                        _ -> Nothing

promoPieces = [Queen, Rook, Bishop, Knight]

getPromo :: IO PieceType
getPromo = do
  putStr "Promote to: "
  i:nput <- getLine
  if ((toUpper i):(map toLower nput)) `elem` map show promoPieces
    then return $ read $ (toUpper i):(map toLower nput)
    else do
    let pieceList = unlines . map show $ promoPieces
    putStrLn ("Piece must be one of the following:\n" ++ pieceList ++ "Try again.")
    getPromo

promoteMaybe :: Board -> Square -> IO Board
promoteMaybe brd sqr =
  case pawnPromote brd sqr of
    Just colr -> do
      t <- getPromo
      return $ insertPiece brd $ Piece t colr sqr sqr
    Nothing -> return brd

type Move = (Square, Square)

forcedMoves :: Board -> Color -> [Square]
forcedMoves brd colr =
  let checks' = checks brd colr
      checkAttackers = case listToMaybe checks' of
                         Nothing -> []
                         Just chk -> 
                           case lookupS brd chk of
                             Nothing -> []
                             Just (Piece _ color' _ _) -> map (targeters validMoves brd color') checks'
  in if not . null $ checks'
     then let (Piece King _ sqr _) = fromJust (king brd colr)
          in sqr:(concat checkAttackers)
     else []

squareSelect :: String -> (Square -> Bool) -> [Square] -> IO (Maybe Square)
squareSelect msg isValid bnds = do
  putStr msg
  sqr <- getLine
  case sqr of
    "cancel" -> return Nothing
    sqr -> case safeSquare bnds sqr of
             Nothing -> do putStrLn "Not a valid square, try again."
                           squareSelect msg isValid bnds
             Just sqr -> if isValid sqr
               then return (Just sqr)
               else do
               putStrLn "Not a valid move, try again."
               squareSelect msg isValid bnds

handleMove :: Board -> Square -> Square -> Board
handleMove brd sqr1@(Square col1 _) sqr2@(Square col2 _) =
  case lookupS brd sqr1 of
    Nothing -> brd
    Just pc@(Piece King _ _ _) ->
      let dx = abs (col1 - col2)
      in if dx > 1
         then castle brd pc sqr2
         else move brd pc sqr2
    Just pc -> move brd pc sqr2
  where castle brd@(Board _ bnds) kng@(Piece King _ (Square col1 row) _) (Square col2 _) =
          let rookCol1 = if col2 > col1 then maxX bnds else minX bnds
              rookCol2 = col2 + signum (col1 - col2)
              rookSqr1 = Square rookCol1 row
              rookSqr2 = Square rookCol2 row
          in handleMove (move brd kng (Square col2 row)) rookSqr1 rookSqr2
            
getMove :: Game -> IO (Maybe Move)
getMove (Game brd trn) = do
  let validSqrs1 = case forcedMoves brd trn of
        [] -> coloredSquares brd trn
        fm -> fm
  sqr1 <- squareSelect "Enter square to move: " (\x -> True) validSqrs1
  case sqr1 of
    Nothing -> return Nothing
    Just s1 -> do
      sqr2 <- squareSelect "Enter destination square: " (notCheck s1) $ validMoves brd $ lookupS' brd s1
      case sqr2 of
        Nothing -> return Nothing
        Just s2 -> return $ Just (s1, s2)
  where notCheck sqr1 sqr2 = null . checks (handleMove brd sqr1 sqr2) $ trn

play :: Game -> IO Game
play game@(Game brd trn) = do
  if checkMate brd trn
    then do
    putStrLn $ show (cycleEnumPred trn) ++ " wins."
    play (Game board White)
    else do
    printGame game
    mv <- getMove game 
    case mv of
      Nothing -> play game
      Just (s1, s2) -> do
        newBrd <- promoteMaybe (handleMove brd s1 s2) s2
        play $ Game newBrd (cycleEnumSucc trn)

main = play (Game board White)
  
