module EngineFunctional (move) where

import           Common        (Board (Board), Color (..), Dest,
                                File (Eight, Five, Four, One), Game (..), MoveF,
                                MoveType (..), Name (Bi, Kg, Kt, Pn, Qn, Rk),
                                Origin, Piece (..), Pos, Rank (A, D, F, H),
                                basicDirections, determineStatus, getPiece,
                                knightDirections, movePiece, nextPiecePos,
                                north, pieceBetween, rankFileDiff, south,
                                standardIf)
import           Control.Arrow ((&&&))
import           Data.List     (elemIndex)
import           Data.Maybe    (fromMaybe, isNothing, mapMaybe)


isStalemate :: Game -> Bool
isStalemate game = not $ or (allPositions >>= canMoveFrom)
    where
        allPositions    = [(r, f) | r <- [A .. H], f <- [One .. Eight]]
        legalMove p1    = maybe False ((/= Illegal) . validate game . calcMoveType game p1)
        canMoveFrom p   = map (legalMove p) ((basicDirections ++ knightDirections) <*> [p])

isCheck :: Game -> Bool
isCheck game = (markers game <$> getKingPos (board game) (color game)) /= Just []

markers :: Game -> Pos -> [Pos]
markers game pos = filter legalMoveFrom (basicPositions ++ knightPositions)
    where
        basicPositions  = mapMaybe (\dir -> nextPiecePos (board game) dir pos) basicDirections
        knightPositions = mapMaybe (\dir -> dir pos) knightDirections
        legalMoveFrom p = Illegal /= calcMoveType (changeTurn game) p pos

kingMoveType :: Piece -> Origin -> Dest -> MoveType
kingMoveType p (r1, f1) (r2, f2)
    | fileDiff <= 1 && rankDiff <= 1 = Standard ((r1, f1), (r2, f2))
    | moveCount p == 0 && fileDiff == 0 && rankDiff == 2 = Castle ((r1, f1), (r2, f2)) (cOrigin, cDest)
    | otherwise = Illegal
    where
        (rankDiff, fileDiff)     = rankFileDiff (r1, f1) (r2, f2)
        cOrigin             = (if r2 > r1 then H else A, f1)
        cDest               = (if r2 > r1 then F else D, f1)

pawnMoveType :: Game -> Piece -> Origin -> Dest -> MoveType
pawnMoveType (Game _ cnt _ b) p (r1, f1) (r2, f2)
    | rankDiff == 0 && fileDiff == 1 && destEmpty && advance = Standard ((r1, f1), (r2, f2))
    | rankDiff == 0 && fileDiff == 2 && destEmpty && advance && moveCount p == 0 = Standard ((r1, f1), (r2, f2))
    | rankDiff == 1 && fileDiff == 1 && advance && capture = Standard ((r1, f1), (r2, f2))
    | rankDiff == 1 && fileDiff == 1 && advance && enPassant = EnPassant ((r1, f1), (r2, f2)) capturedPos
    | otherwise = Illegal
    where
        (rankDiff, fileDiff) = rankFileDiff (r1, f1) (r2, f2)
        capturedPos          = (r2, if f2 > f1 then Five else Four)
        advance              = (f1 < f2) == (pieceColor p == Wht)
        destPiece            = getPiece b (r2, f2)
        destEmpty            = isNothing destPiece
        capture              = maybe False ((/= pieceColor p) . pieceColor) destPiece
        enPassant = fromMaybe False $ do
            behind <- if f1 < f2 then south (r2, f2) else north (r2, f2)
            Piece mc pcol nm <- getPiece b behind
            return $ mc == cnt - 1 && pcol /= pieceColor p && nm == Pn

calcMoveType :: Game -> Pos -> Pos -> MoveType
calcMoveType (Game s cnt col b) p1 p2
    | not (noBlock && colorOkay && correctTurn) = Illegal
    | otherwise = maybe Illegal f pc1
    where
        (pc1, pc2)           = (getPiece b p1, getPiece b p2)
        (rankDiff, fileDiff) = rankFileDiff p1 p2
        correctTurn          = (pieceColor <$> pc1) == Just col
        noBlock              = (name <$> pc1) == Just Kt || not (pieceBetween b p1 p2)
        colorOkay            = (pieceColor <$> pc1) == Just col && (pieceColor <$> pc1) /= (pieceColor <$> pc2)
        f p = case name p of
            Rk -> standardIf (rankDiff == 0 || fileDiff == 0) p1 p2
            Kt -> standardIf (fileDiff * rankDiff == 2) p1 p2
            Bi -> standardIf (fileDiff == rankDiff) p1 p2
            Qn -> standardIf (rankDiff == 0 || fileDiff == 0 || fileDiff == rankDiff) p1 p2
            Kg -> kingMoveType p p1 p2
            Pn -> pawnMoveType (Game s cnt col b) p p1 p2

validate :: Game -> MoveType -> MoveType
validate game moveType = if validMove then moveType else Illegal
    where
        testGame = performMove game moveType
        notCheck = not $ isCheck (changeTurn testGame)
        validMove = notCheck && case moveType of
            Castle (ori, dest) (_, cdest) -> null ([ori, dest, cdest] >>= markers game)
            _ -> True

move :: MoveF
move game (p1, p2) =
    case validate game (calcMoveType game p1 p2) of
        Illegal -> game
        mt      -> performMove game mt

performMove :: Game -> MoveType -> Game
performMove game mt = Game newStatus newCount newCol newBoard
    where
        newStatus    = uncurry determineStatus . (isCheck &&& isStalemate) $ Game (status game) newCount newCol newBoard
        newCount     = count game + 1
        newCol       = if color game == Wht then Blk else Wht
        newBoard     = movePiece game mt

changeTurn :: Game -> Game
changeTurn game = game { color = if color game == Wht then Blk else Wht }

getKingPos :: Board -> Color -> Maybe Pos
getKingPos (Board b) col = do
    let nameBoard = map (map (fmap (\p -> (pieceColor p, name p)))) b
    let kingPiece = Just (col, Kg)
    fidx <- elemIndex True $ map (elem kingPiece) nameBoard
    ridx <- elemIndex kingPiece $ nameBoard !! fidx
    return (toEnum ridx, toEnum (7 - fidx))
