module ChessData (
    Game (..), Name(..), Color(..), Piece(..), Rank(..), File(..),
    MoveType(..), Pos, Origin, Dest, Status(..), Board(..), MoveF,
    MoveInfo(..), RunEngine
) where


data Name = Rk | Kn | Bi | Qn | Kg | Pn                                     deriving (Show, Enum, Eq, Ord)
data Color = Blk | Wht                                                      deriving (Show, Eq)
data Piece = Piece { moveCount :: Int, pieceColor :: Color, name :: Name }  deriving (Show, Eq)
data Rank = A | B | C | D | E | F | G | H                                   deriving (Show, Enum, Eq, Ord)
data File = One | Two | Three | Four | Five | Six | Seven | Eight           deriving (Show, Enum, Eq, Ord)
data Status = Checkmate | Check | Stalemate | Boring                        deriving (Eq)
data MoveType = Illegal
              | Standard Pos Pos
              | EnPassant Pos Pos Pos
              | Castle Pos Pos Pos Pos                                      deriving (Show, Eq)

data Game = Game {
    status :: Status,
    count  :: Int,
    color  :: Color,
    board  :: Board
} deriving (Eq)

data MoveInfo = MoveInfo {
    posDifference     :: (Int, Int),
    pieces            :: (Maybe Piece, Maybe Piece),
    piecePositions    :: (Pos, Pos),
    blocked           :: Bool,
    validColors       :: Bool,
    castleOrigin      :: Pos,
    castleDestination :: Pos,
    pawnCapturedPos   :: Pos,
    isAdvance         :: Bool,
    pawnCapture       :: Bool,
    overshoot         :: Bool
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

