module ReaderEngine where

import           Common
import           Control.Concurrent   ()
import           Control.Monad.Reader (forM, when)
import           Control.Monad.State  (MonadState (get, put), State, execState,
                                       gets, modify)
import           Data.Maybe           (catMaybes, fromMaybe, isJust, isNothing,
                                       mapMaybe)


runEngine :: Game -> [(Origin, Dest)] -> Game
runEngine = foldl (\ acc a -> execState (move a) acc)

determineStatus :: Bool -> Bool -> Status
determineStatus inCheck inStalemate
    | inCheck && inStalemate  = Checkmate
    | inCheck                 = Check
    | inStalemate             = Stalemate
    | otherwise               = Boring

isStalemate :: State Game Bool
isStalemate = do
    game <- get
    let allPositions  = [(r, f) | r <- [A .. H], f <- [One .. Eight]]
    let allDirections = basicDirections ++ knightDirections
    let allMoves p    = map (calcMoveType game p) (catMaybes $ allDirections <*> [p])
    all (== Illegal) <$> forM (allPositions >>= allMoves) validate

isStalemate' :: State Game [MoveType]
isStalemate' = do
    game <- get
    let allPositions  = [(r, f) | r <- [A .. H], f <- [One .. Eight]]
    let allDirections = basicDirections ++ knightDirections
    let allMoves p    = map (calcMoveType game p) (catMaybes $ allDirections <*> [p])
    forM (allPositions >>= allMoves) validate

isCheck :: State Game Bool
isCheck = do
    kPos <- activeKingPos
    maybe (pure False) (fmap (not . null) . markers) kPos

isCheck' :: State Game [Pos]
isCheck' = do
    kPos <- activeKingPos
    maybe (pure []) markers kPos

markers :: Pos -> State Game [Pos]
markers pos = do
    changeTurn
    game <- get
    let basicPositions  = mapMaybe (\dir -> nextPiecePos (board game) dir pos) basicDirections
        knightPositions = mapMaybe (\dir -> dir pos) knightDirections
        legalMoveFrom p = Illegal /= calcMoveType game p pos
        threats         = filter legalMoveFrom (basicPositions ++ knightPositions)
    changeTurn
    return threats


kingMoveType :: Piece -> Origin -> Dest -> MoveType
kingMoveType p (r1, f1) (r2, f2)
    | fileDiff <= 1 && rankDiff <= 1 = Standard (r1, f1) (r2, f2)
    | moveCount p == 0 && fileDiff == 0 && rankDiff == 2 = Castle (r1, f1) (r2, f2) castleOrigin castleDest
    | otherwise = Illegal
    where
        (rankDiff, fileDiff)     = rankFileDiff (r1, f1) (r2, f2)
        castleOrigin             = (if r2 > r1 then H else A, f1)
        castleDest               = (if r2 > r1 then F else D, f1)

pawnMoveType :: Game -> Piece -> Origin -> Dest -> MoveType
pawnMoveType game p (r1, f1) (r2, f2)
    | rankDiff == 0 && fileDiff == 1 && destEmpty && advance = Standard (r1, f1) (r2, f2)
    | rankDiff == 0 && fileDiff == 2 && destEmpty && advance && moveCount p == 0 = Standard (r1, f1) (r2, f2)
    | rankDiff == 1 && fileDiff == 1 && advance && capture = Standard (r1, f1) (r2, f2)
    | rankDiff == 1 && fileDiff == 1 && advance && enPassant = EnPassant (r1, f1) (r2, f2) capturedPos
    | otherwise = Illegal
    where
        (rankDiff, fileDiff) = rankFileDiff (r1, f1) (r2, f2)
        capturedPos          = (r2, if f2 > f1 then Five else Four)
        advance              = (f1 < f2) == (pieceColor p == Wht)
        destPiece            = getPiece (board game) (r2, f2)
        destEmpty            = isNothing destPiece
        capture              = maybe False ((/= pieceColor p) . pieceColor) destPiece
        enPassant = fromMaybe False $ do
            behind <- if f1 < f2 then south (r2, f2) else north (r2, f2)
            Piece mc pcol nm <- getPiece (board game) behind
            return $ mc == count game - 1 && pcol /= pieceColor p && nm == Pn

calcMoveType :: Game -> Pos -> Pos -> MoveType
calcMoveType game p1 p2
    | not (noBlock && colorOkay) = Illegal
    | otherwise = maybe Illegal f pc1
    where
        (pc1, pc2)           = (getPiece (board game) p1, getPiece (board game) p2)
        (rankDiff, fileDiff) = rankFileDiff p1 p2
        noBlock              = (name <$> pc1) == Just Kn || not (pieceBetween game p1 p2)
        colorOkay            = (pieceColor <$> pc1) == Just (color game) && (pieceColor <$> pc1) /= (pieceColor <$> pc2)
        f p = case name p of
            Rk -> standardIf (rankDiff == 0 || fileDiff == 0) p1 p2
            Kn -> standardIf (fileDiff * rankDiff == 2) p1 p2
            Bi -> standardIf (fileDiff == rankDiff) p1 p2
            Qn -> standardIf (rankDiff == 0 || fileDiff == 0 || fileDiff == rankDiff) p1 p2
            Kg -> kingMoveType p p1 p2
            Pn -> pawnMoveType game p p1 p2

castleThroughCheck :: MoveType -> State Game Bool
castleThroughCheck (Castle ori dest _ cdest) = not . all null <$> forM [ori, dest, cdest] markers
castleThroughCheck _ = return False

validate :: MoveType -> State Game MoveType
validate moveType = do
    game <- get
    performMove moveType
    invalidMove <- isCheck
    -- invalidMove <- (||) <$> isCheck <*> castleThroughCheck moveType
    put game
    return $ if invalidMove then Illegal else moveType

move :: (Pos, Pos) -> State Game ()
move (p1, p2) = do
    game     <- get
    moveType <- validate (calcMoveType game p1 p2)
    when (moveType /= Illegal) $ do
        performMove moveType
        incrementCount
        changeTurn
        updateStatus

pieceBetween :: Game -> Pos -> Pos -> Bool
pieceBetween game p1 p2 = any (isJust . getPiece (board game)) $ positionsBetween p1 p2

positionsBetween :: Pos -> Pos -> [Pos]
positionsBetween (r1, f1) (r2, f2)
    | r1 == r2 = [(r1, f) | f <- orderRange f1 f2]
    | f1 == f2 = [(r, f1) | r <- orderRange r1 r2]
    | fileDiff == rankDiff = zip (orderRange r1 r2) (orderRange f1 f2)
    | otherwise = []
    where
        orderRange a b       = init . tail $ if a < b then [a .. b] else reverse [b .. a]
        (rankDiff, fileDiff) = rankFileDiff (r1, f1) (r2, f2)

performMove :: MoveType -> State Game ()
performMove mt = do
    (brd, cnt) <- gets ((,) . board) <*> gets count
    let oriPiece ori = (\pc -> pc { moveCount = cnt }) <$> getPiece brd ori
        newBoard     = setPieces brd $ case mt of
            Illegal                 -> []
            Standard ori des        -> [(Nothing, ori), (oriPiece ori, des)]
            EnPassant ori des cpos  -> [(Nothing, cpos), (Nothing, ori), (oriPiece ori, des)]
            Castle ori des co cd    -> [(Nothing, co), (Nothing, ori), (oriPiece ori, des), (getPiece brd co, cd)]

    modify $ \g -> g { board = newBoard }


changeTurn, incrementCount, updateStatus :: State Game ()
changeTurn     = modify $ \ g -> g { color = if color g == Wht then Blk else Wht }
incrementCount = modify $ \ g -> g { count = 1 + count g }
updateStatus   = do
    newStatus <- determineStatus <$> isCheck <*> isStalemate
    modify $ \g -> g { status = newStatus }
