module EngineMonadic (move) where

import           Common               (Color (Blk, Wht), File (Five, Four),
                                       Game (board, color, count, status),
                                       MoveF, MoveInfo (..), MoveType (..),
                                       Name (Bi, Kg, Kt, Pn, Qn, Rk),
                                       Piece (Piece, moveCount, name, pieceColor),
                                       Pos, Rank (A, D, F, H), activeKingPos,
                                       basicDirections, determineStatus,
                                       getPiece, knightDirections, movePiece,
                                       nextPiecePos, north, pieceBetween,
                                       posPiecePairs, rankFileDiff, south)
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
        pieceName           = name <$> pc1,
        piecePositions      = (p1, p2),
        blocked             = (name <$> pc1) /= pure Kt && pieceBetween (board game) p1 p2,
        emptyDestination    = isNothing pc2,
        validColors         = (pieceColor <$> pc1) == Just (color game) && (pieceColor <$> pc1) /= (pieceColor <$> pc2),
        castleRanks         = if fst p2 > fst p1 then ((H, snd p1), (F, snd p1)) else ((A, snd p1), (D, snd p1)),
        overshotPos         = (fst p2, if snd p2 > snd p1 then Five else Four),
        isAdvance           = (snd p1 < snd p2) == ((pieceColor <$> pc1) == pure Wht),
        overshoot           = isOvershoot game p1 p2,
        firstMove           = (moveCount <$> pc1) == Just 0
    }

isOvershoot :: Game -> Pos -> Pos -> Bool
isOvershoot game p1 p2 = fromMaybe False $ do
    Piece _ qcol _ <- getPiece (board game) p1
    behind <- if snd p1 < snd p2 then south p2 else north p2
    Piece mc pcol nm <- getPiece (board game) behind
    return $ mc == count game - 1 && pcol /= qcol && nm == Pn

cMoveInfo :: MoveInfo -> MoveType
cMoveInfo (MoveInfo _ Nothing _ _ _ _ _ _ _ _ _) = Illegal
cMoveInfo (MoveInfo (dx, dy) (Just n) ps block emptyDes validCol cstPs ovrPos adv ovShoot fstMove)
    | block || not validCol                                        = Illegal
    | n == Rk && (dx == 0 || dy == 0)                              = Standard  ps
    | n == Kt && dy * dx == 2                                      = Standard  ps
    | n == Bi && dy == dx                                          = Standard  ps
    | n == Qn && (dx == 0 || dy == 0 || dy == dx)                  = Standard  ps
    | n == Kg && dy <= 1 && dx <= 1                                = Standard  ps
    | n == Pn && dx == 0 && dy == 1 && emptyDes && adv             = Standard  ps
    | n == Pn && dx == 0 && dy == 2 && emptyDes && adv && fstMove  = Standard  ps
    | n == Pn && dx == 1 && dy == 1 && adv && not emptyDes         = Standard  ps
    | n == Pn && dx == 1 && dy == 1 && adv && ovShoot              = EnPassant ps ovrPos
    | n == Kg && fstMove && dy == 0 && dx == 2                     = Castle    ps cstPs
    | otherwise                                                    = Illegal

invalidCastle :: MoveType -> State Game Bool
invalidCastle (Castle (ori, dest) (_, cdest)) = not . all null <$> forM [ori, dest, cdest] markers
invalidCastle _ = return False

validate :: MoveType -> State Game MoveType
validate moveType = do
    game <- get
    updateBoard moveType
    invalidMove <- (||) <$> isCheck <*> invalidCastle moveType
    put game
    return $ if invalidMove then Illegal else moveType

move :: MoveF
move g (p1, p2) = flip execState g $ do
    moveType <- calcMoveType p1 p2 >>= validate
    when (moveType /= Illegal) $ updateGame moveType

updateGame :: MoveType -> State Game ()
updateGame mt = do
    updateBoard mt
    updateCount
    changeTurn
    updateStatus

updateBoard :: MoveType -> State Game ()
updateBoard mt = modify $ \g -> g { board = movePiece g mt}

changeTurn, updateCount, updateStatus :: State Game ()
changeTurn     = modify $ \ g -> g { color = if color g == Wht then Blk else Wht }
updateCount = modify $ \ g -> g { count = 1 + count g }
updateStatus   = get >>= \g -> determineStatus <$> isCheck <*> isStalemate >>= (\s -> put g { status = s })
