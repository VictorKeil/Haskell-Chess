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

data PieceType = Pawn deriving (Show)

data Piece = Piece PieceType Color Square deriving (Show)

type MovePredicates = (Square, [Maybe Piece -> Bool])
type MoveList = [(Square, [MovePredicates])]

type Board = HMap.Map Square Piece

genBoard :: Board
genBoard = let whitePawns = [(Square x 6, Piece Pawn White (Square x 6)) | x <- [1..8]]
               blackPawns = [(Square x 7, Piece Pawn Black (Square x 7)) | x <- [1..8]]
           in HMap.fromList (whitePawns ++ blackPawns)

checkMovePreds :: Board -> MovePredicates -> Bool
checkMovePreds b (s, fs) = and $ map ($ (HMap.lookup s b)) fs

globalValidMoves :: Board -> MoveList -> [Square]
globalValidMoves b ml = map fst $ filter (and . (map $ checkMovePreds b) . snd) ml

sameColor :: Color -> Maybe Piece -> Bool
sameColor _ Nothing = False
sameColor col1 (Just (Piece _ col2 _)) = col1 == col2

preValidMoves :: Piece -> MoveList
preValidMoves (Piece Pawn color (Square col row)) =
  let (direction, initRow) = if color == White then (1, 2) else (-1, 7)
      basePredicates = [null]
      baseSquares = 
        if row == initRow
        then [Square col (row + direction * x) | x <- [1..2]]
        else [Square col (row + direction)]
      baseMoves = [(last xs, [(x, basePredicates) | x <- xs]) | xs <- tail . inits $ baseSquares]

      capturePredicates = [not . null, not . sameColor color]
      captureSquares =
        [Square (col - 1) (row + direction), Square (col + 1) (row + direction)]
      captureMoves = map (\s -> (s, [(s, capturePredicates)])) captureSquares
     in baseMoves ++ captureMoves


board = genBoard
p1 = Piece Pawn White (Square 5 6)
p2 = Piece Pawn Black (Square 5 7)

