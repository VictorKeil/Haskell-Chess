module Test where

import Chess
import Data.List

testBounds :: Piece -> [Square]
testBounds pc = let (Board _ bnds) = emptyBoard
                    ts = targetSquares emptyBoard pc
                    vm = validMoves emptyBoard pc
                in (ts \\ restrictToBounds bnds ts) ++ (vm \\ restrictToBounds bnds vm)

testPieces :: [Piece]
testPieces = nubBy (\(Piece t1 _ _ _) (Piece t2 _ _ _) -> t1 == t2) $ concat
  $ testP [Piece t b s s | t <- [minBound .. maxBound::PieceType]
                       , b <- [Black]
                       , s <- [Square col row | col <- [1..8], row <- [1..8]]]
  where testP [] = [] 
        testP (p:ps) = (if null (testBounds p) then [] else [p] ):(testP ps)
          
applyPair f p = f (fst p) (snd p)
mapPair f (a, b) = (f a, f b)
k1 = (Piece King White (Square 5 1) (Square 5 1))
testBoard = foldr (flip insertPiece) emptyBoard
  [k1
  ,Piece Rook White (Square 8 1) (Square 8 1)
  ,Piece Rook White (Square 1 1) (Square 1 1)
  ,Piece Rook Black (Square 3 4) (Square 1 1)]

