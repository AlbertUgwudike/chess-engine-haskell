module Common (
    module ChessData,
    getPiece, activeKingPos, setPiece, north, south, rankFileDiff, startGame, mateGame,
    setPieces, standardIf, nextPiecePos, basicDirections, knightDirections, emptyBoard, startBoard
) where


import           ChessData
import           Control.Arrow       ((&&&))
import           Control.Monad       ((>=>))
import           Control.Monad.State (State, gets)
import           Data.Foldable       (find)
import           Data.Maybe          (catMaybes, isJust)

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

activeKingPos :: State Game (Maybe Pos)
activeKingPos = do
    activeColor <- gets color
    let positions        = [(rank, file) | file <- [Eight, Seven ..], rank <- [A ..]]
        getPiecePosPairs = catMaybes . zipWith (fmap . (,)) positions . concat . pMatrix . board
        isActiveKing     = (== (Kg, activeColor)) . (name &&& pieceColor)
    gets $ fmap fst . find (isActiveKing . snd) . getPiecePosPairs


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

startGame :: Game
startGame = Game Boring 0 Wht startBoard

mateBoard :: Board
mateBoard = setPieces emptyBoard
            [ (Just (Piece 0 Wht Kg), (A, One)),
                (Just (Piece 0 Blk Rk), (B, Three)),
                (Just (Piece 0 Blk Rk), (A, Four))
            ]

mateGame :: Game
mateGame = Game Checkmate 0 Wht mateBoard
