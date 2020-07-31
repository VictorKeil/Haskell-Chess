module Test where

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
          
applyPair f p = f (fst p) (snd p)
mapPair f (a, b) = (f a, f b)
testBoard = foldl (\brd p -> applyPair (move brd) $ mapPair strSqr p) board [("f2", "f3")
                                                                ,("e7", "e5")
                                                                ,("g2", "g4")
                                                                ,("d8", "h4")]
