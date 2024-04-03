module ChessData (
    Game (..), Name(..), Color(..), Piece(..), Rank(..), File(..), MoveType(..), Pos, Origin, Dest, Status(..), Board(..)
) where


data Name = Rk | Kn | Bi | Qn | Kg | Pn                                     deriving (Show, Enum, Eq, Ord)
data Color = Blk | Wht                                                      deriving (Show, Eq)
data Piece = Piece { moveCount :: Int, pieceColor :: Color, name :: Name }  deriving (Show, Eq)
data Rank = A | B | C | D | E | F | G | H                                   deriving (Show, Enum, Eq, Ord)
data File = One | Two | Three | Four | Five | Six | Seven | Eight           deriving (Show, Enum, Eq, Ord)
data Status = Checkmate | Check | Stalemate | Boring
data MoveType = Illegal
              | Standard Pos Pos
              | EnPassant Pos Pos Pos
              | Castle Pos Pos Pos Pos                                      deriving (Show, Eq)

newtype Board = Board { pMatrix :: [[Maybe Piece]] }

type Pos = (Rank, File)
type Origin = Pos
type Dest = Pos

data Game = Game {
    status :: Status,
    count  :: Int,
    color  :: Color,
    board  :: Board
}

instance Show Game where
    show game = unlines [moveMsg, specialMsg, show $ board game]
        where
            col = if color game == Wht then "White" else "Black"
            moveMsg = col ++ " to move."
            specialMsg = case status game of
                Checkmate -> col ++ " Checkmate!"
                Check     -> col ++ " Check!"
                Stalemate -> col ++ " Stalemate!"
                Boring    -> col ++ " Boring!"

instance Show Board where
    show (Board b) = unlines . concatMap (foldl1 hcat . map showPiece) $ b
        where showPiece (Just (Piece _ col nm)) = pad (" " ++ show col ++ show nm ++ " ")
              showPiece Nothing = pad space
              space = "       "
              pad n = hcat (replicate 4 "|") [replicate 7 '-', space, n, space]


hcat :: [String] -> [String] -> [String]
hcat = zipWith (++)

