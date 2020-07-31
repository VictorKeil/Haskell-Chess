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

data Piece = Piece PieceType Color Square deriving (Show)

data Bounds = Bounds {minX :: Int, minY :: Int, maxX :: Int, maxY :: Int} deriving (Show)

square :: Piece -> Square
square (Piece _ _ sqr) = sqr

pieceSym :: Piece -> Char
pieceSym (Piece Knight _ _) = 'N'
pieceSym (Piece kind _ _) = head $ show kind

type BoardMap = HMap.Map Square Piece

data Board = Board BoardMap Bounds
  
lookupS :: Board -> Square -> Maybe Piece
lookupS (Board brd _) sqr = HMap.lookup sqr brd

showSquaresWith :: Board -> Char -> [Square] -> String
showSquaresWith (Board brd (Bounds minX minY maxX maxY)) showChar sqrs =
  let width = maxX - minX + 1
      height = maxY - minY + 1

      -- Make row and column coordinate axes
      cols = [toEnum (x + 64) :: Char | x <- [1 .. width]]
      rows = [head . show $ x | x <- [height, height - 1 .. 1]]

      -- Generate a list of strings that alternate between '.' and '#'
      -- Each string represents a row on the chess board
      textSquares =
        [[if odd (x+y) then ' ' else '#' | x <- [1 .. width]] | y <- [1 .. height]]

      -- Generate a list with similar structure as the one above, but fill it with
      -- corresponding Square objects
      squares = reverse [[(Square x y) | x <- [1 .. width]] | y <- [1 .. height]]
      -- Zip the previous two lists together to create a list of list of pairs of
      -- empty square characters and Square objects
      textSqrPairs = zipWith zip textSquares squares
      -- Map through each sublist, look for a piece on the board on each square.
      -- If a piece is found, replace that character with the pieceSym of the piece.
      -- If a piece is not found, simply return the empty square character of that square.
      filledBoard =
        (flip map) textSqrPairs $ map (\pair -> if (snd pair) `elem` sqrs
                                                then showChar
                                                else case HMap.lookup (snd pair) brd of
                                                       Nothing -> fst pair
                                                       Just piece -> pieceSym piece)
      axesBoard = (transpose $ rows:(replicate 8 ' '):(transpose filledBoard))
  in unlines $ (map (intersperse '|') axesBoard) ++ [(replicate 10 ' '), "    " ++ intersperse '|' cols]

showSquares :: Board -> [Square] -> String
showSquares brd = showSquaresWith brd 'X'

instance Show Board where
  show brd = showSquares brd []

printSquares :: Board -> [Square] -> IO ()
printSquares brd sqrs = do
  ANSI.clearScreen
  putStrLn $ showSquares brd sqrs

sameColor :: BoardMap -> Color -> Square -> Bool
sameColor brd col1 sqr = case HMap.lookup sqr brd of
                          Nothing -> False
                          Just (Piece _ col2 _) -> col1 == col2

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
targetSquares (Board brd bnds) (Piece Pawn color (Square col row)) =
  let (direction, initRow) = if color == White then (1,2) else (-1, 7)
  in restrictToBounds bnds
     $ [Square (col - 1) (row + direction), Square (col + 1) (row + direction)]
  
targetSquares (Board brd bnds) (Piece Rook color (Square col row)) =
  let posX = [Square (col + x) row | x <- [1..maxX bnds - col]]
      negX = [Square (col - x) row | x <- [1..col - minX bnds]]
      posY = [Square col (row + y) | y <- [1..maxY bnds - row]]
      negY = [Square col (row - y) | y <- [1..row - minY bnds]]
  in concat $ map (takeLong brd) $ [negX, posX, negY, posY]

targetSquares (Board brd bnds) (Piece Bishop color (Square col row)) =
  let maxNorth = maxY bnds - row
      maxSouth = row - minY bnds
      maxWest = col - minX bnds
      maxEast = maxX bnds - col
      
      ne = [Square (col + x) (row + x) | x <- [1 .. min maxNorth maxEast]]
      se = [Square (col + x) (row - x) | x <- [1 .. min maxSouth maxEast]]
      sw = [Square (col - x) (row - x) | x <- [1 .. min maxSouth maxWest]]
      nw = [Square (col - x) (row + x) | x <- [1 .. min maxNorth maxWest]]
  in concat $ map (takeLong brd) $ [ne, se, sw, nw]

targetSquares (Board brd bnds) (Piece Knight color (Square col row)) =
  let set1 = [Square (col + x) (row + y) | x <- [-1, 1], y <- [-2, 2]]
      set2 = [Square (col + x) (row + y) | x <- [-2, 2], y <- [-1,1]]
  in restrictToBounds bnds $ set1 ++ set2

targetSquares brd (Piece Queen color sqr) =
  concat $ map (targetSquares brd) [(Piece Rook color sqr), (Piece Bishop color sqr)]

targetSquares (Board brd bnds) (Piece King color (Square col row)) =
  let set1 = [Square (col + x) (row + y) | x <- [-1..1], y <- [-1,1]]
  in restrictToBounds bnds $ set1 ++ [Square (col + x) row | x <- [-1,1]]

-- Takes a function and its argument, applies the function to the argument and stores
-- the result and the argument in a pair
fArgPair :: (a -> b) -> a -> (b, a)
fArgPair f x = (f x, x)

width :: Bounds -> Int
width (Bounds minX _ maxX _) = maxX - minX + 1

height :: Bounds -> Int
height (Bounds _ minY _ maxY) = maxY - minY + 1

targeters :: (Board -> Piece -> [Square]) -> Board -> Color -> Square -> [Square]
targeters moveFunc brd color sqr =
  let pieces = [Piece t color sqr | t <- [minBound..maxBound::PieceType]]
      pieceTargets :: [([Square], Piece)]
      pieceTargets = map (fArgPair $ moveFunc brd) pieces
  in concat $ map occupiedBy pieceTargets
  where occupiedBy (sqrs, pc) = filter (isEnemyOf pc) sqrs
          where isEnemyOf (Piece t c _) sqr' = case lookupS brd sqr' of
                                                 Nothing -> False
                                                 Just (Piece t' c' _) -> (t == t') && (c /= c')
      
targeted :: Board -> Color -> Square -> Bool
targeted brd color sqr = not . null $ targeters targetSquares brd color sqr

validMoves :: Board -> Piece -> [Square]
validMoves (Board brd bnds) (Piece Pawn color (Square col row)) =
  let (direction, initRow) = if color == White then (1,2) else (-1,7)
      baseSquares =
        if row == initRow
        then [Square col (row + direction * x) | x <- [1..2]]
        else [Square col (row + direction)]
      baseMoves = takeWhile (nullSquare brd) baseSquares
      
      tgtSqrs = targetSquares (Board brd bnds) (Piece Pawn color (Square col row))
      validCaptures = filter (\s -> not (sameColor brd color s || nullSquare brd s)) tgtSqrs
  in restrictToBounds bnds $ validCaptures ++ baseMoves

validMoves (Board brd bnds) (Piece King color sqr) =
  let tgtSqrs = targetSquares (Board brd bnds) (Piece King color sqr)
      diffColor = filter (not . sameColor brd color) $ tgtSqrs
  in filter (not . targeted (Board brd bnds) color) diffColor
  
validMoves (Board brd bnds) (Piece ptype color sqr) =
  filter (not . sameColor brd color) $ targetSquares (Board brd bnds) (Piece ptype color sqr)

coloredPieces :: Board -> Color -> [Piece]
coloredPieces (Board brd _) color =
  map snd $ filter ((\(Piece _ color' _) -> color' == color) . snd) $ HMap.toList brd


coloredSquares :: Board -> Color -> [Square]
coloredSquares brd color = map (\(Piece _ _ sqr) -> sqr) $ coloredPieces brd color

king :: Board -> Color -> Maybe Piece
king brd colr = find (\(Piece t _ _) -> t == King) $ coloredPieces brd colr

checks :: Board -> Color -> [Square]
checks brd colr = 
  case king brd colr of
    Nothing -> []
    Just (Piece _ _ sqr) -> targeters targetSquares brd colr sqr

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
                                  Just (Piece _ color' _) -> map (targeters validMoves brd color') checks'
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
insertPiece (Board brd bnds) (Piece t c sqr) =
  (Board (HMap.insert sqr (Piece t c sqr) brd) bnds)

removePiece :: Board -> Piece -> Board
removePiece (Board brd bnds) (Piece _ _ sqr) =
  (Board (HMap.delete sqr brd) bnds)

printMoves :: Board -> Piece -> IO ()
printMoves brd pc = printSquares (insertPiece brd pc) $ validMoves brd pc

board = foldr (flip insertPiece) emptyBoard pieces
  where pieces = let whitePRow = 2
                     blackPRow = 7
                     whitePawns = [Piece Pawn White (Square col whitePRow) | col <- [1..8]]
                     blackPawns = [Piece Pawn Black (Square col blackPRow) | col <- [1..8]]
                     whiteRooks = [Piece Rook White (Square col 1) | col <- [1,8]]
                     blackRooks = [Piece Rook Black (Square col 8) | col <- [1,8]]
                     whiteKnights = [Piece Knight White (Square col 1) | col <- [2,7]]
                     blackKnights = [Piece Knight Black (Square col 8) | col <- [2,7]]
                     whiteBishops = [Piece Bishop White (Square col 1) | col <- [3, 6]]
                     blackBishops = [Piece Bishop Black (Square col 8) | col <- [3, 6]]
                     whiteQueen = [Piece Queen White (Square 4 1)]
                     blackQueen = [Piece Queen Black (Square 4 8)]
                     whiteKing = [Piece King White (Square 5 1)]
                     blackKing = [Piece King Black (Square 5 8)]
          in (whitePawns ++ whiteRooks ++ whiteKnights ++ whiteBishops ++ whiteQueen ++ whiteKing)
          ++ (blackPawns ++ blackRooks ++ blackKnights ++ blackBishops ++ blackQueen ++ blackKing)

move :: Board -> Square -> Square -> Board
move brd sqr1 sqr2 = case lookupS brd sqr1 of
                 Nothing -> brd
                 Just (Piece t c _) ->
                   insertPiece (removePiece brd (Piece t c sqr1)) (Piece t c sqr2)

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
             Just sqr -> return (Just sqr)

cycleEnumSucc :: (Eq a, Enum a, Bounded a) => a -> a
cycleEnumSucc e = if e == maxBound then minBound else succ e

cycleEnumPred :: (Eq a, Enum a, Bounded a) => a -> a
cycleEnumPred e = if e == minBound then maxBound else pred e

data Game = Game Board Color
instance Show Game where
  show (Game _ trn) = "Game - Turn: " ++ show trn

printGame :: Game -> IO ()
printGame (Game brd trn) = do
  printSquares brd []
  putStrLn ("Turn: " ++ show trn)

pawnPromote :: Board -> Square -> Maybe Color
pawnPromote brd sqr = case lookupS' brd sqr of
                        (Piece Pawn White (Square _ row)) ->
                          if row == 8 then Just White else Nothing
                        (Piece Pawn Black (Square _ row)) ->
                          if row == 1 then Just Black else Nothing
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
      return $ insertPiece brd $ Piece t colr sqr
    Nothing -> return brd

play :: Game -> IO Game
play (Game brd trn) = do
  if checkMate brd trn
    then do
    putStrLn $ show (cycleEnumPred trn) ++ " wins."
    play (Game board White)
    else do
    printGame (Game brd trn)
    sqr1 <- squareSelect "Enter square to move: " $ coloredSquares brd trn
    case sqr1 of
      Nothing -> play (Game brd trn)
      Just s1 -> do
        putStr $ showSquaresWith brd '.' [s1]
        sqr2 <- squareSelect "Enter destination square: " $ validMoves brd $ lookupS' brd s1
        case sqr2 of
          Nothing -> play (Game brd trn)
          Just s2 -> do
            newBrd <- promoteMaybe (move brd s1 s2) s2
            play $ Game newBrd (cycleEnumSucc trn)

main = play (Game board White)
  
