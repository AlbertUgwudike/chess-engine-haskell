module ChessData (
    Name(..), Color(..), Piece(..), Rank(..), File(..), MoveType(..), Pos, Origin, Dest, Status(..), Board,
    getPiece, getKingPos, setPiece, north, south, rankFileDiff,
    setPieces, standardIf, nextPiecePos, basicDirections, knightDirections, emptyBoard, startBoard
) where


import           Control.Monad ((>=>))
import           Data.List     (elemIndex)
import           Data.Maybe    (isJust)

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

newtype Board = Board [[Maybe Piece]]

type Pos = (Rank, File)
type Origin = Pos
type Dest = Pos

instance Show Board where
    show (Board b) = unlines . concatMap (foldl1 hcat . map showPiece) $ b
        where showPiece (Just (Piece _ color nm)) = pad (" " ++ show color ++ show nm ++ " ")
              showPiece Nothing = pad space
              space = "       "
              pad n = hcat (replicate 4 "|") [replicate 7 '-', space, n, space]


hcat :: [String] -> [String] -> [String]
hcat = zipWith (++)


rankFileDiff :: Origin -> Dest -> (Int, Int)
rankFileDiff (r1, f1) (r2, f2) =
    let (fi1, fi2)   = (fromEnum f1, fromEnum f2)
        (ri1, ri2)   = (fromEnum r1, fromEnum r2)
    in  (abs $ ri1 - ri2, abs $ fi1 - fi2)

go :: (Pos -> Bool) -> (Rank -> Rank, File -> File) -> Pos -> Maybe Pos
go outBounds (rf, ff) (r, f) = if outBounds (r, f) then Nothing else Just (rf r, ff f)

north, east, south, west :: Pos -> Maybe Pos
north = go (\p -> snd p == Eight) (id, succ)
south = go (\p -> snd p == One)   (id, pred)
east  = go (\p -> fst p == H)     (succ, id)
west  = go (\p -> fst p == A)     (pred, id)

basicDirections :: [Pos -> Maybe Pos]
basicDirections = [ north, east, south, west ] ++ [ a >=> b | a <- [east, west], b <- [north, south]]

knightDirections :: [Pos -> Maybe Pos]
knightDirections = concat [[ a >=> a >=> b, a >=> b >=> b ] | a <- [east, west], b <- [north, south]]

rowOf :: Maybe Piece -> [Maybe Piece]
rowOf = replicate 8

order :: [Name]
order = [Rk, Kn, Bi, Qn, Kg, Bi, Kn, Rk]

emptyBoard :: Board
emptyBoard = Board $ replicate 8 $ replicate 8 Nothing

startBoard :: Board
startBoard = let pieceSet col = [map (Just . col) order, rowOf (Just $ col Pn)]
                 empty = replicate 4 $ rowOf Nothing
             in  Board $ pieceSet (Piece 0 Blk) ++ empty ++ reverse (pieceSet (Piece 0 Wht))

getPiece :: Board -> Pos -> Maybe Piece
getPiece (Board mat) (r, f) = mat !! (7 - fromEnum f) !! fromEnum r

getKingPos :: Board -> Color -> Maybe Pos
getKingPos (Board b) col = do
    let nameBoard = map (map (fmap (\p -> (pieceColor p, name p)))) b
    let kingPiece = Just (col, Kg)
    fidx <- elemIndex True $ map (elem kingPiece) nameBoard
    ridx <- elemIndex kingPiece $ nameBoard !! fidx
    return (toEnum ridx, toEnum (7 - fidx))

setAt :: Int -> a -> [a] -> [a]
setAt i n arr = take i arr ++ [n] ++ drop (i + 1) arr

-- siuuuuuuuu
setPiece :: Board -> Maybe Piece -> Pos -> Board
setPiece (Board mat) new (r, f) = Board $ setAt fidx newRow mat
    where ridx = fromEnum r
          fidx = 7 - fromEnum f
          newRow = setAt ridx new (mat !! fidx)

setPieces :: Board -> [(Maybe Piece, Pos)] -> Board
setPieces = foldl (\ b (mp, pos) -> setPiece b mp pos)


standardIf :: Bool -> Pos -> Pos -> MoveType
standardIf flag p1 p2 = if flag then Standard p1 p2 else Illegal

nextPiecePos :: Board -> (Pos -> Maybe Pos) -> Pos -> Maybe Pos
nextPiecePos b dir pos
    | isJust piece = nextPos
    | otherwise = nextPos >>= nextPiecePos b dir
    where nextPos = dir pos
          piece = nextPos >>= getPiece b
