data Square = Square Int Int

instance Show Square where
  show (Square col row) = toEnum (col + 64) : show row

data Color = Black | White deriving (Show)

data Pawn = Pawn Color Square deriving (Show)
