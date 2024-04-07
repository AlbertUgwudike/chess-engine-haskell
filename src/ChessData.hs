module ChessData (
    Game (..), Name(..), Color(..), Piece(..), Rank(..), File(..),
    MoveType(..), Pos, Origin, Dest, Status(..), Board(..), MoveF,
    MoveInfo(..), RunEngine
) where


data Name = Rk | Kt | Bi | Qn | Kg | Pn                                     deriving (Show, Enum, Eq, Ord)
data Color = Blk | Wht                                                      deriving (Show, Eq)
data Piece = Piece { moveCount :: Int, pieceColor :: Color, name :: Name }  deriving (Show, Eq)
data Rank = A | B | C | D | E | F | G | H                                   deriving (Show, Enum, Eq, Ord)
data File = One | Two | Three | Four | Five | Six | Seven | Eight           deriving (Enum, Eq, Ord)
data Status = Checkmate | Check | Stalemate | Boring                        deriving (Eq)
data MoveType = Illegal
              | Standard (Pos, Pos)
              | EnPassant (Pos, Pos) Pos
              | Castle (Pos, Pos) (Pos, Pos)                                deriving (Show, Eq)

data Game = Game {
    status :: Status,
    count  :: Int,
    color  :: Color,
    board  :: Board
} deriving (Eq)

data MoveInfo = MoveInfo {
    posDifference    :: (Int, Int),
    pieceName        :: Maybe Name,
    piecePositions   :: (Pos, Pos),
    blocked          :: Bool,
    emptyDestination :: Bool,
    validColors      :: Bool,
    castleRanks      :: (Pos, Pos),
    overshotPos      :: Pos,
    isAdvance        :: Bool,
    overshoot        :: Bool,
    firstMove        :: Bool
}

newtype Board = Board { pMatrix :: [[Maybe Piece]] }                        deriving (Eq)

type Pos = (Rank, File)
type Origin = Pos
type Dest = Pos
type MoveF = Game -> (Pos, Pos) -> Game
type RunEngine = MoveF -> Game -> [(Origin, Dest)] -> Game

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
              hcat = zipWith (++)

instance Show File where
    show file = case file of
        One   -> "1"
        Two   -> "2"
        Three -> "3"
        Four  -> "4"
        Five  -> "5"
        Six   -> "6"
        Seven -> "7"
        Eight -> "8"
