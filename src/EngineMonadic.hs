module EngineMonadic where

import           Common
import           Control.Arrow        ((&&&))
import           Control.Monad.Reader (filterM, forM, when)
import           Control.Monad.State  (MonadState (get, put), State, execState,
                                       gets, modify, (>=>))
import           Data.Maybe           (fromMaybe, isNothing, mapMaybe)


isStalemate :: State Game Bool
isStalemate = do
    allPositions  <- gets $ map fst . posPiecePairs
    let allDirections = map (\dir p -> (p, dir p)) (basicDirections ++ knightDirections)
    let movePairs     = mapMaybe sequence $ allDirections <*> allPositions
    all (==Illegal) <$> forM movePairs (uncurry calcMoveType >=> validate)

isCheck :: State Game Bool
isCheck = do
    kPos <- activeKingPos
    maybe (pure False) (fmap (not . null) . markers) kPos

markers :: Pos -> State Game [Pos]
markers pos = do
    let knightPositions = mapMaybe (\dir -> dir pos) knightDirections
    basicPositions <- gets $ \g -> mapMaybe (\dir -> nextPiecePos (board g) dir pos) basicDirections
    changeTurn
    threats <- filterM (\p -> (/= Illegal) <$> calcMoveType p pos) (basicPositions ++ knightPositions)
    changeTurn
    return threats

calcMoveType :: Pos -> Pos -> State Game MoveType
calcMoveType p1 p2 = do
    let gameToPiece pos = flip getPiece pos . board
    (pc1, pc2) <- gets $ gameToPiece p1 &&& gameToPiece p2
    gets $ \game -> cMoveInfo MoveInfo {
        posDifference       = rankFileDiff p1 p2,
        pieces              = (pc1, pc2),
        piecePositions      = (p1, p2),
        blocked             = (name <$> pc1) /= pure Kn && pieceBetween (board game) p1 p2,
        validColors         = (pieceColor <$> pc1) == Just (color game) && (pieceColor <$> pc1) /= (pieceColor <$> pc2),
        castleOrigin        = (if fst p2 > fst p1 then H else A, snd p1),
        castleDestination   = (if fst p2 > fst p1 then F else D, snd p1),
        pawnCapturedPos     = (fst p2, if snd p2 > snd p1 then Five else Four),
        isAdvance           = (snd p1 < snd p2) == ((pieceColor <$> pc1) == pure Wht),
        pawnCapture         = ((/=) . pieceColor <$> pc1 <*> (pieceColor <$> pc2)) == pure True,
        overshoot           = isOvershoot game p1 p2
    }

isOvershoot :: Game -> Pos -> Pos -> Bool
isOvershoot game p1 p2 = fromMaybe False $ do
    Piece _ qcol _ <- getPiece (board game) p1
    behind <- if snd p1 < snd p2 then south p2 else north p2
    Piece mc pcol nm <- getPiece (board game) behind
    return $ mc == count game - 1 && pcol /= qcol && nm == Pn

cMoveInfo :: MoveInfo -> MoveType
cMoveInfo (MoveInfo (rd, fd) (pc1, pc2) (p1, p2) bk vc co cd pcp ia pc os) = moveType
    where
        moveType = if not bk && vc then maybe Illegal f pc1 else Illegal
        destEmpty = isNothing pc2
        f p | name p == Rk && (rd == 0 || fd == 0)                                      = Standard  p1 p2
            | name p == Kg && fd * rd == 2                                              = Standard  p1 p2
            | name p == Bi && fd == rd                                                  = Standard  p1 p2
            | name p == Qn && (rd == 0 || fd == 0 || fd == rd)                          = Standard  p1 p2
            | name p == Kg && fd <= 1 && rd <= 1                                        = Standard  p1 p2
            | name p == Pn && rd == 0 && fd == 1 && destEmpty && ia                     = Standard  p1 p2
            | name p == Pn && rd == 0 && fd == 2 && destEmpty && ia && moveCount p == 0 = Standard  p1 p2
            | name p == Pn && rd == 1 && fd == 1 && ia && pc                            = Standard  p1 p2
            | name p == Pn && rd == 1 && fd == 1 && ia && os                            = EnPassant p1 p2 pcp
            | name p == Kg && moveCount p == 0 && fd == 0 && rd == 2                    = Castle    p1 p2 co cd
            | otherwise                                                                 = Illegal

castleThroughCheck :: MoveType -> State Game Bool
castleThroughCheck (Castle ori dest _ cdest) = not . all null <$> forM [ori, dest, cdest] markers
castleThroughCheck _ = return False

validate :: MoveType -> State Game MoveType
validate moveType = do
    game <- get
    performMove moveType
    invalidMove <- (||) <$> isCheck <*> castleThroughCheck moveType
    put game
    return $ if invalidMove then Illegal else moveType

move :: MoveF
move g (p1, p2) = flip execState g $ do
    moveType <- calcMoveType p1 p2 >>= validate
    when (moveType /= Illegal) $ do
        performMove moveType
        incrementCount
        changeTurn
        updateStatus

performMove :: MoveType -> State Game ()
performMove mt = modify $ \g -> g { board = movePiece g mt}

changeTurn, incrementCount, updateStatus :: State Game ()
changeTurn     = modify $ \ g -> g { color = if color g == Wht then Blk else Wht }
incrementCount = modify $ \ g -> g { count = 1 + count g }
updateStatus   = do
    newStatus <- determineStatus <$> isCheck <*> isStalemate
    modify $ \g -> g { status = newStatus }
