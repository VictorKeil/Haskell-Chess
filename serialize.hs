{-# LANGUAGE ScopedTypeVariables #-}
module Serialize where

import Data.Word
import Data.Int
import Control.Monad
import Data.Serialize
import Data.Serialize.Put
import qualified Data.HashMap as HMap

import Chess

putSquare :: Putter Square
putSquare (Square col row) = do
  put (fromIntegral col :: Int32)
  put (fromIntegral row :: Int32)

getSquare :: Get Square
getSquare = do
  col <- fromIntegral <$> getInt32be :: Get Int
  row <- fromIntegral <$> getInt32be :: Get Int
  return $ Square col row

instance Serialize Square where
  put = putSquare
  get = getSquare

putEnum8 :: forall a. Enum a => Putter a
putEnum8 e = put (fromIntegral $ fromEnum e :: Word8)

getEnum8 :: forall a. Enum a => Get a
getEnum8 = do
  c <- getWord8
  case fromIntegral c of
    c' | c' `elem` map fromEnum (enumFrom $ toEnum 0 :: [a]) -> return $ toEnum c'
       | otherwise -> fail $ "Value not in enum range: '" ++ show c ++ "'"

instance Serialize Color where
  put = putEnum8
  get = getEnum8

instance Serialize PieceType where
  put = putEnum8
  get = getEnum8

putPiece :: Putter Piece
putPiece (Piece pt colr sqr sqr') = do
  put pt
  put colr
  put sqr
  put sqr'

getPiece :: Get Piece
getPiece = do
  pt <- get :: Get PieceType
  colr <- get :: Get Color
  sqr <- get :: Get Square
  sqr' <- get :: Get Square
  return (Piece pt colr sqr sqr')

instance Serialize Piece where
  put = putPiece
  get = getPiece

putBounds :: Putter Bounds
putBounds (Bounds minX minY maxX maxY) = do
  let vals = [minX, minY, maxX, maxY]
  foldM (\_ v -> put (fromIntegral v :: Int32)) () vals

getBounds :: Get Bounds
getBounds = do
  minX <- fromIntegral <$> (get :: Get Int32)
  minY <- fromIntegral <$> (get :: Get Int32)
  maxX <- fromIntegral <$> (get :: Get Int32)
  maxY <- fromIntegral <$> (get :: Get Int32)
  return (Bounds minX minY maxX maxY)

instance Serialize Bounds where
  put = putBounds
  get = getBounds

putBoard :: Putter Board
putBoard (Board bmap bnds) = do
  put $ HMap.toList bmap
  put bnds

getBoard :: Get Board
getBoard = do
  bmap <- get :: Get [(Square, Piece)]
  bnds <- get :: Get Bounds
  return $ Board (HMap.fromList bmap) bnds
  
instance Serialize Board where
  put = putBoard
  get = getBoard
  
