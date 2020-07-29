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

data Color = Black | White deriving (Show, Eq)

data PieceType = King | Queen | Pawn | Rook | Bishop | Knight deriving (Eq, Show, Enum, Bounded)

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

targetedSquares :: Board -> Color -> [([Square], Piece)]
targetedSquares (Board brd bnds) color =
  let enemySqrs :: [(Square, Piece)]
      enemySqrs = filter (not . sameColor brd color . fst) $ HMap.toList brd
      targetedBy :: [([Square], Piece)]
      targetedBy = map (fArgPair (targetSquares (Board brd bnds)) . snd) enemySqrs
  in targetedBy

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
  in restrictToBounds bnds $ validCaptures ++ baseMoves

validMoves (Board brd bnds) (Piece King color sqr) =
  let tgtSqrs = targetSquares (Board brd bnds) (Piece King color sqr)
      diffColor = filter (not . sameColor brd color) $ tgtSqrs
  in filter (not . ((flip elem) $ concat $ map fst $ targetedSquares (Board brd bnds) color)) diffColor
  
validMoves (Board brd bnds) (Piece ptype color sqr) =
  filter (not . sameColor brd color) $ targetSquares (Board brd bnds) (Piece ptype color sqr)

checks :: Board -> Piece -> [Square]
checks brd (Piece King color sqr) =
  let checks = filter ((sqr `elem`) . fst) $ targetedSquares brd color 
  in map ((\(Piece _ _ sqr) -> sqr) . snd) checks

checkMate :: Board -> Piece -> Bool
checkMate brd (Piece King color sqr) =
  let kingCanMove = not . null $ validMoves brd (Piece King color sqr)
      checks' = checks brd (Piece King color sqr)
      friendlyTargets = if null checks'
                        then []
                        else case lookupS brd (head checks') of
                               Nothing -> []
                               Just (Piece _ checkColor _) ->
                                 concat $ map fst $ targetedSquares brd checkColor
      canKillCheck = length (friendlyTargets \\ checks') == 1
  in not . or $ [null checks', kingCanMove, canKillCheck]

strSqr :: String -> Square
strSqr (a:n:rs) = Square (fromEnum a - 64) (fromEnum n - 48)

emptyBoard :: Board
emptyBoard = Board HMap.empty (Bounds 1 1 8 8)

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

testBounds :: Piece -> [Square]
testBounds pc = let (Board _ bnds) = emptyBoard
                    ts = targetSquares emptyBoard pc
                    vm = validMoves emptyBoard pc
                in (ts \\ restrictToBounds bnds ts) ++ (vm \\ restrictToBounds bnds vm)

testPieces :: [Piece]
testPieces = nubBy (\(Piece t1 _ _) (Piece t2 _ _) -> t1 == t2) $ concat
  $ testP [Piece t b s | t <- [minBound .. maxBound::PieceType]
                       , b <- [Black]
                       , s <- [Square col row | col <- [1..8], row <- [1..8]]]
  where testP [] = [] 
        testP (p:ps) = (if null (testBounds p) then [] else [p] ):(testP ps)
          

board = genBoard (Bounds 1 1 8 8)
k1 = Piece King Black (Square 1 5)

