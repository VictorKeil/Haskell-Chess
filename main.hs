module Main where

import Chess
import Console
import Network

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



playNetwork = do
  (sock, (myColr, oppColr)) <- initN "localhost" "30001"
  case myColr of
    _| myColr == White -> play (Game board Nothing (cycle [WPlayer $ ConsoleP White, WPlayer $ NetworkP sock Black]))
     | otherwise -> play (Game board Nothing (cycle [WPlayer $ NetworkP sock White, WPlayer $ ConsoleP Black]))

playOffline = play (Game board Nothing (cycle $ map WPlayer [ConsoleP White, ConsoleP Black]))
  
  
main = playNetwork
