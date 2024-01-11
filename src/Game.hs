module Game where

import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes, listToMaybe)
import Data.Functor ((<&>))
import ChessData

data Game = Game { count :: Int, color :: Color, board :: Board }                         

instance Show Game where
    show game@(Game cnt c b) = unlines [moveMsg, specialMsg, show b]
        where col = if c == Wht then "White" else "Black"
              moveMsg = col ++  " to move."
              specialMsg = case status game of
                  Checkmate -> col ++ " Checkmate!"
                  Check     -> col ++ " Check!"
                  Stalemate -> col ++ " Stalemate!"
                  Boring    -> ""

runEngine :: Game -> [(Origin, Dest)] -> Game
runEngine game moves = foldl move game moves

status :: Game -> Status  
status game
    | inCheck && inStalemate = Checkmate
    | inCheck = Check
    | inStalemate = Stalemate
    | otherwise = Boring
    where (inCheck, inStalemate) = (isCheck game, isStalemate game)


startGame :: Game
startGame = Game 0 Wht startBoard

mateGame :: Game
mateGame = Game 0 Wht $ setPieces emptyBoard $ 
        [
            (Just (Piece 0 Wht Kg), (A, One)),
            (Just (Piece 0 Blk Rk), (B, Three)),
            (Just (Piece 0 Blk Rk), (A, Four))
        ]


isStalemate :: Game -> Bool
isStalemate game = not $ any id (allPositions >>= canMoveFrom)
    where allPositions = [(r, f) | r <- [A .. H], f <- [One .. Eight]]
          legalMove p1 p2 = fromMaybe False $ (/= Illegal) . validate game . calcMoveType game p1 <$> p2
          canMoveFrom p = map (legalMove p) ((basicDirections ++ knightDirections) <*> [p])


isCheckmate :: Game -> Bool
isCheckmate game = fromMaybe False $ do
    kingPos <- getKingPos (board game) (color game) 
    markingPosition <- listToMaybe (markers game kingPos) 

    let moveKingPos dir = dir kingPos
    let kingPositions = catMaybes $ map moveKingPos basicDirections
    let legalKingMove pos = (/= Illegal) . validate game $ calcMoveType game kingPos pos
    let betweens = (markingPosition : positionsBetween kingPos markingPosition)

    let noKingMoves = null $ filter legalKingMove kingPositions
    let noBlockOrCapture = null . filter (/= kingPos) $ betweens >>= markers (changeTurn game)
    let multipleMarkers = (length $ markers game kingPos) > 1

    return $ noKingMoves && (multipleMarkers || noBlockOrCapture)


isCheck :: Game -> Bool
isCheck game = (markers game <$> getKingPos (board game) (color game)) /= Just []


markers :: Game -> Pos -> [Pos]
markers game pos = filter legalMoveFrom (basicPositions ++ knightPositions)
    where basicPositions  = catMaybes . map (\ dir -> nextPiecePos (board game) dir pos ) $ basicDirections
          knightPositions = catMaybes . map (\ dir -> dir pos ) $ knightDirections
          legalMoveFrom p = Illegal /= calcMoveType (changeTurn game) p pos


kingMoveType :: Game -> Piece -> Origin -> Dest -> MoveType
kingMoveType (Game cnt col b) p (r1, f1) (r2, f2)
    | fileDiff <= 1 && rankDiff <= 1 = Standard (r1, f1) (r2, f2)
    | moveCount p == 0 && fileDiff == 0 && rankDiff == 2 = Castle (r1, f1) (r2, f2) castleOrigin castleDest
    | otherwise = Illegal

    where (rankDiff, fileDiff) = rankFileDiff (r1, f1) (r2, f2)
          castleOrigin = (if r2 > r1 then H else A, f1)
          castleDest   = (if r2 > r1 then F else D, f1)


pawnMoveType :: Game -> Piece -> Origin -> Dest -> MoveType
pawnMoveType (Game cnt col b) p (r1, f1) (r2, f2)
    | rankDiff == 0 && fileDiff == 1 && destEmpty && advance = Standard (r1, f1) (r2, f2)
    | rankDiff == 0 && fileDiff == 2 && destEmpty && advance && moveCount p == 0 = Standard (r1, f1) (r2, f2)
    | rankDiff == 1 && fileDiff == 1 && advance && capture = Standard (r1, f1) (r2, f2)
    | rankDiff == 1 && fileDiff == 1 && advance && enPassant = EnPassant (r1, f1) (r2, f2) capturedPos
    | otherwise = Illegal
    
    where (rankDiff, fileDiff) = rankFileDiff (r1, f1) (r2, f2)
          capturedPos  = (r2, if f2 > f1 then Five else Four)
          advance = (f1 < f2) == (pieceColor p == Wht)
          destPiece = getPiece b (r2, f2)
          destEmpty = isNothing destPiece
          capture = fromMaybe False ((/= pieceColor p) . pieceColor <$> destPiece)
          enPassant = fromMaybe False $ do
              behind <- if f1 < f2 then south (r2, f2) else north (r2, f2)
              Piece mc pcol name <- getPiece b behind
              return $ mc == cnt - 1 && pcol /= pieceColor p && name == Pn


calcMoveType :: Game -> Pos -> Pos -> MoveType
calcMoveType game@(Game cnt col b) p1 p2
    | not noBlock || not colorOkay = Illegal
    | otherwise = fromMaybe Illegal $ pc1 <&> \ p -> case name p of
        Rk -> standardIf (rankDiff == 0 || fileDiff == 0) p1 p2
        Kn -> standardIf (fileDiff * rankDiff == 2) p1 p2
        Bi -> standardIf (fileDiff == rankDiff) p1 p2
        Qn -> standardIf (rankDiff == 0 || fileDiff == 0 || fileDiff == rankDiff) p1 p2
        Kg -> kingMoveType (Game cnt col b) p p1 p2
        Pn -> pawnMoveType (Game cnt col b) p p1 p2
   
    where [pc1, pc2] = map (getPiece b) [p1, p2]
          (rankDiff, fileDiff) = rankFileDiff p1 p2
          noBlock = (name <$> pc1) == Just Kn || (not $ pieceBetween game p1 p2)
          colorOkay = (pieceColor <$> pc1) == Just col && (pieceColor <$> pc1) /= (pieceColor <$> pc2) 


validate :: Game -> MoveType -> MoveType
validate game moveType = if validMove then moveType else Illegal
    where testGame = performMove game moveType 
          notCheck = not $ isCheck testGame
          validMove = notCheck && case moveType of
              Castle ori dest _ cdest -> null ([ori, dest, cdest] >>= markers game)
              _ -> True


move :: Game -> (Pos, Pos) -> Game
move game (p1, p2) = case validate game $ calcMoveType game p1 p2 of
    Illegal -> game
    mt -> changeTurn $ performMove game mt


pieceBetween :: Game -> Pos -> Pos -> Bool
pieceBetween (Game _ _ b) p1 p2 = any (isJust . getPiece b) $ positionsBetween p1 p2


positionsBetween :: Pos -> Pos -> [Pos]
positionsBetween (r1, f1) (r2, f2)
    | r1 == r2 = [(r1, f) | f <- orderRange f1 f2]
    | f1 == f2 = [(r, f1) | r <- orderRange r1 r2]
    | fileDiff == rankDiff = zip (orderRange r1 r2) (orderRange f1 f2)
    | otherwise = []
    where orderRange a b = init . tail $ if a < b then [a .. b] else reverse [b .. a]
          (rankDiff, fileDiff) = rankFileDiff (r1, f1) (r2, f2)


performMove :: Game -> MoveType -> Game
performMove (Game cnt col b) mt = Game newCount col newBoard
    where oriPiece ori = getPiece b ori <&> \ (Piece _ c n) -> Piece cnt c n
          newCount = cnt + 1
          newBoard = setPieces b $ case mt of
              Illegal -> [  ]
              Standard ori des -> [ (Nothing, ori), (oriPiece ori, des) ]
              EnPassant ori des cpos -> [ (Nothing, cpos), (Nothing, ori), (oriPiece ori, des) ]
              Castle ori des co cd -> [ (Nothing, co), (Nothing, ori), (oriPiece ori, des), (getPiece b co, cd) ] 


changeTurn :: Game -> Game
changeTurn (Game cnt Blk b) = Game cnt Wht b
changeTurn (Game cnt Wht b) = Game cnt Blk b