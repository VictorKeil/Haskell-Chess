import Data.List
import qualified Data.HashMap as HMap
import Data.Hashable

data Square = Square Int Int deriving (Eq, Ord)

instance Show Square where
  show (Square col row) = toEnum (col + 64) : show row

instance Hashable Square where
  hash (Square col row) = col + row * 8
  hashWithSalt _ sqr = hash sqr

data Color = Black | White deriving (Show, Eq)

data PieceType = King | Queen | Pawn | Rook | Bishop | Knight deriving (Show)

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

showSquares :: Board -> [Square] -> String
showSquares (Board brd (Bounds minX minY maxX maxY)) sqrs =
  let width = maxX - minX + 1
      height = maxY - minY + 1

      -- Make row and column coordinate axes
      cols = ' ':[toEnum (x + 64) :: Char | x <- [1 .. width]]
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
                                                then 'X'
                                                else case HMap.lookup (snd pair) brd of
                                                       Nothing -> fst pair
                                                       Just piece -> pieceSym piece)
      axesBoard = (transpose $ rows:(transpose filledBoard)) ++ [cols]
  in unlines $ map (intersperse '|') axesBoard

instance Show Board where
  show brd = showSquares brd []

printSquares :: Board -> [Square] -> IO ()
printSquares brd sqrs = do putStrLn $ showSquares brd sqrs

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

targetSquares :: Board -> Piece -> [Square]
targetSquares (Board brd bnds) (Piece Pawn color (Square col row)) =
  let (direction, initRow) = if color == White then (1,2) else (-1, 7)
  in [Square (col - 1) (row + direction), Square (col + 1) (row + direction)]
  
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
      
      ne = [Square (col + x) (row + x) | x <- [1 .. max maxNorth maxEast]]
      se = [Square (col + x) (row - x) | x <- [1 .. max maxSouth maxEast]]
      sw = [Square (col - x) (row - x) | x <- [1 .. max maxSouth maxWest]]
      nw = [Square (col - x) (row + x) | x <- [1 .. max maxNorth maxWest]]
  in concat $ map (takeLong brd) $ [ne, se, sw, nw]

targetSquares (Board brd bnds) (Piece Knight color (Square col row)) =
  let set1 = [Square (col + x) (row + y) | x <- [-1, 1], y <- [-2, 2]]
      set2 = [Square (col + x) (row + y) | x <- [-2, 2], y <- [-1,1]]
  in set1 ++ set2

targetSquares brd (Piece Queen color sqr) =
  concat $ map (targetSquares brd) [(Piece Rook color sqr), (Piece Bishop color sqr)]

targetSquares (Board brd bnds) (Piece King color (Square col row)) =
  let set1 = [Square (col + x) (row + y) | x <- [-1..1], y <- [-1,1]]
  in set1 ++ [Square (col + x) row | x <- [-1,1]]

targetedSquares :: Board -> Color -> [Square]
targetedSquares (Board brd bnds) color =
  let enemySqrs = filter (not . sameColor brd color . fst) $ HMap.toList brd
  in concat $ map (targetSquares (Board brd bnds) . snd) enemySqrs

validMoves :: Board -> Piece -> [Square]
validMoves (Board brd bnds) (Piece Pawn color (Square col row)) =
  let (direction, initRow) = if color == White then (1,2) else (-1, 7)
      baseSquares =
        if row == initRow
        then [Square col (row + direction * x) | x <- [1..2]]
        else [Square col (row + direction)]
      baseMoves = takeWhile (nullSquare brd) baseSquares
      
      tgtSqrs = targetSquares (Board brd bnds) (Piece Pawn color (Square col row))
      validCaptures = filter (\s -> not (sameColor brd color s || nullSquare brd s)) tgtSqrs
  in validCaptures ++ baseMoves

validMoves (Board brd bnds) (Piece King color sqr) =
  let tgtSqrs = targetSquares (Board brd bnds) (Piece King color sqr)
      nonTargeted = filter (not . (`elem` targetedSquares (Board brd bnds) color)) tgtSqrs
  in filter (not . sameColor brd color) $ nonTargeted
  
validMoves (Board brd bnds) (Piece ptype color sqr) =
  filter (not . sameColor brd color) $ targetSquares (Board brd bnds) (Piece ptype color sqr)

check :: Board -> Piece -> Bool
check brd (Piece King color sqr) = sqr `elem` targetedSquares brd color

strSqr :: String -> Square
strSqr (a:n:rs) = Square (fromEnum a - 64) (fromEnum n - 48)

genBoard :: Bounds -> Board
genBoard bnds = let whitePawns = [(Square x 2, Piece Pawn White (Square x 2)) | x <- [1..8]]
                    blackPawns = [(Square x 7, Piece Pawn Black (Square x 7)) | x <- [1..8]]
                    queen = [(Square 2 5, Piece Queen White (Square 2 5))]
                in Board (HMap.fromList (whitePawns ++ blackPawns ++ queen)) bnds

moves :: Board -> Square -> [Square]
moves brd sqr = case lookupS brd sqr of Nothing -> []
                                        Just p -> validMoves brd p

insertPiece :: Board -> Piece -> Board
insertPiece (Board brd bnds) (Piece t c sqr) =
  (Board (HMap.insert sqr (Piece t c sqr) brd) bnds)

printMoves :: Board -> Piece -> IO ()
printMoves brd pc = printSquares (insertPiece brd pc) $ validMoves brd pc

board = genBoard (Bounds 1 1 8 8)

