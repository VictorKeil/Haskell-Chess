{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ExistentialQuantification #-}
module Chess where

import Data.Maybe
import Data.List.Index
import Data.Char
import Data.Tuple
import Data.List
import qualified Data.HashMap as HMap
import Data.Hashable
import Control.Applicative

import Utils

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

type BoardMap = HMap.Map Square Piece

data Board = Board BoardMap Bounds

type Move = (Square, Square)

class CriticalError e where
  trigger :: e -> String

class CriticalError ce => MoveError e ce | e -> ce where
  handleError :: e -> Either ce Game

class MoveError e ce => Player p e ce | p -> e where
  color :: p -> Color
  getMove :: p -> Game -> IO (Either e Move)

data WPlayer = forall p e ce. Player p e ce => WPlayer p

data Game = Game Board (Maybe Move) [WPlayer]
instance Show Game where
  show (Game _ _ ps) = "Game - Turn: " ++ case listToMaybe ps of
                                          Nothing -> ""
                                          Just (WPlayer p) -> show $ color p

width :: Bounds -> Int
width (Bounds minX _ maxX _) = maxX - minX + 1

height :: Bounds -> Int
height (Bounds _ minY _ maxY) = maxY - minY + 1

square :: Piece -> Square
square (Piece _ _ sqr _) = sqr

pieceSym :: Piece -> Char
pieceSym (Piece Knight _ _ _) = 'N'
pieceSym (Piece kind _ _ _) = head $ show kind

lookupS :: Board -> Square -> Maybe Piece
lookupS (Board brd _) sqr = HMap.lookup sqr brd

sameColor :: Board -> Color -> Square -> Bool
sameColor brd col1 sqr = case lookupS brd sqr of
                          Nothing -> False
                          Just (Piece _ col2 _ _) -> col1 == col2

nullSquare :: Board -> Square -> Bool
nullSquare brd sqr = null $ lookupS brd sqr

-- Move mechanics for long range pieces
takeLong :: Board -> [Square] -> [Square]
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
  
targetSquares brd@(Board _ bnds) (Piece Rook color (Square col row) _) =
  let posX = [Square (col + x) row | x <- [1..maxX bnds - col]]
      negX = [Square (col - x) row | x <- [1..col - minX bnds]]
      posY = [Square col (row + y) | y <- [1..maxY bnds - row]]
      negY = [Square col (row - y) | y <- [1..row - minY bnds]]
  in concat $ map (takeLong brd) $ [negX, posX, negY, posY]

targetSquares brd@(Board _ bnds) (Piece Bishop color (Square col row) _) =
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

validCastles :: Board -> Piece -> [Square]
validCastles brd@(Board _ bnds) k@(Piece King colr (Square col row) _) =
  if moved (Just k)
  then []
  else let left = col - 2
           right = col + 2
           rookSqrs = [Square (minX bnds) row, Square (maxX bnds) row]
           rooksMoved = map (moved . lookupS brd) rookSqrs
           castleSqrs = [[Square x row | x <- [left .. col]]
                        ,[Square x row | x <- [col .. right]]]
           rookValid = zipFilter (map not rooksMoved) castleSqrs
           canCastle = map (not . any (liftA2 (||) (not . nullSquare brd) (targeted brd colr))) rookValid
           
       in zipFilter canCastle [Square left row, Square right row]
           
           
  where moved (Just (Piece _ _ sqr lSqr)) = sqr /= lSqr
        moved Nothing = True

validMoves :: Board -> Piece -> [Square]
validMoves brd@(Board brdMap bnds) pc@(Piece Pawn color (Square col row) lSqr) =
  let (direction, initRow) = if color == White then (1,2) else (-1,7)
      baseSquares =
        if row == initRow
        then [Square col (row + direction * x) | x <- [1..2]]
        else [Square col (row + direction)]
      baseMoves = takeWhile (nullSquare brd) baseSquares
      
      tgtSqrs = targetSquares brd (Piece Pawn color (Square col row) lSqr)
      -- validCaptures = filter (not . ((||) <$> sameColor brd color <*> nullSquare brd)) tgtSqrs
      validCaptures = filter (not . liftA2 (||) (sameColor brd color) (nullSquare brd)) tgtSqrs
      preCheck = restrictToBounds bnds $ validCaptures ++ baseMoves
  in filter (\s -> not $ check (move brd pc s) color) preCheck

validMoves brd (Piece King color sqr lSqr) =
  let tgtSqrs = targetSquares brd (Piece King color sqr lSqr)
      castles = validCastles brd (Piece King color sqr lSqr)
      diffColor = filter (not . sameColor brd color) $ tgtSqrs
  in castles ++ filter (not . targeted brd color) diffColor
  
validMoves brd pc@(Piece ptype colr sqr lSqr) =
  let preCheck = filter (not . sameColor brd colr) $ targetSquares brd pc
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

validMove :: Board -> Color -> Move -> Bool
validMove brd colr (s1, s2) =
  case lookupS brd s1 of
    Nothing -> False
    Just pc@(Piece _ c sqr _)| c == colr -> s2 `elem` validMoves brd pc
                             |otherwise -> False

handleMove :: Board -> Square -> Square -> Board
handleMove brd sqr1@(Square col1 _) sqr2@(Square col2 _) =
  case lookupS brd sqr1 of
    Nothing -> error ("No piece present on " ++ show sqr1 ++ ".")
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
            
play :: Game -> IO Game
play game@(Game brd lastMv ((WPlayer p):ps)) = do
  if checkMate brd (color p)
    then return (Game brd lastMv [WPlayer p])
    else do
    failableMv <- getMove p game
    case failableMv of
      Left e ->
        case handleError e of
          Left ce -> do
            putStrLn $ trigger ce
            return game
          Right game -> play game
      Right (s1, s2) -> do
        newBrd <- promoteMaybe (handleMove brd s1 s2) s2
        play $ Game newBrd (Just (s1, s2)) ps

