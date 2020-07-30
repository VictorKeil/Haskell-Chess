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
  in filter (not . targeted (Board brd bnds) color) diffColor
  
validMoves (Board brd bnds) (Piece ptype color sqr) =
  filter (not . sameColor brd color) $ targetSquares (Board brd bnds) (Piece ptype color sqr)

checks :: Board -> Piece -> [Square]
checks brd (Piece King color sqr) = targeters validMoves brd color sqr

coloredPieces :: Board -> Color -> [Piece]
coloredPieces (Board brd _) color =
  map snd $ filter ((\(Piece _ color' _) -> color' == color) . snd) $ HMap.toList brd


coloredSquares :: Board -> Color -> [Square]
coloredSquares brd color = map (\(Piece _ _ sqr) -> sqr) $ coloredPieces brd color

checkMate :: Board -> Color -> Bool
checkMate brd color =
  case find (\(Piece t _ _) -> t == King) $ coloredPieces brd color of
    Nothing -> False
    Just king ->
      let kingCanMove = not . null $ validMoves brd king
          checks' = checks brd king
          checkAttackers = if null checks' then []
                           else case lookupS brd (head checks') of
                                  Nothing -> []
                                  Just (Piece _ color' _) -> map (targeters validMoves brd color') checks'
          canKillCheck = 1 == (length . filter (not . null) $ checkAttackers)
      in not . or $ [null checks', kingCanMove, canKillCheck]

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

infoString = "Type in mv to make a move."

safeSquare :: [Square] -> String -> Maybe Square
safeSquare bnds [a,n] = let sqr = strSqr [a,n]
                        in if elem sqr bnds then Just sqr else Nothing
safeSquare _ _ = Nothing

squareSelect :: String -> [Square] -> IO Square
squareSelect msg bnds = do
  putStr msg
  sqr <- getLine
  case safeSquare bnds sqr of
    Nothing -> do putStrLn "Not a valid square, try again."
                  squareSelect msg bnds
    Just sqr -> return sqr

lookupS' :: Board -> Square -> Piece
lookupS' brd sqr = case lookupS brd sqr of
                     Nothing -> error ("The square '" ++ show sqr ++ "' does not contain a piece.")
                     Just sqr -> sqr

play :: Board -> IO Board
play brd = do
  let squares = coloredSquares brd White
      checkMateB = if checkMate brd Black then "Black" else ""
      checkMateW = if checkMate brd White then "White" else ""
  if not . null $ (checkMateB ++ checkMateW)
    then putStrLn ("Check mate: " ++ checkMateB ++ checkMateW)
    else return ()
  putStr $ show brd
  sqr1 <- squareSelect "Enter square to move: " squares
  putStr $ showSquaresWith brd '.' [sqr1]
  sqr2 <- squareSelect "Enter destination square: " $ validMoves brd $ lookupS' brd sqr1
  play $ move brd sqr1 sqr2

main = play board
  
