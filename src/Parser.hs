module Parser (parseMove, ParserAction (..), catMoves) where

import           Common
import           Data.Char  (ord)
import           Data.Maybe (fromMaybe)

data ParserAction = Resign | ParseFail | Move (Pos,  Pos) deriving (Eq, Show)

parseFile :: Char -> Maybe File
parseFile s
    | ord s < ord '1' || ord s > ord '8' = Nothing
    | otherwise = Just ([One .. Eight] !! (ord s - ord '1'))

parseRank :: Char -> Maybe Rank
parseRank s
    | ord s < ord 'A' || ord s > ord 'H' = Nothing
    | otherwise = Just ([A .. H] !! (ord s - ord 'A'))

parseMove :: String -> ParserAction

parseMove "Resign" =  Resign

-- two comma separated coordinates
parseMove [ora, ofi, ',', ' ', dra, dfi] = fromMaybe ParseFail t
    where t = do r1 <- parseRank ora
                 r2 <- parseRank dra
                 f1 <- parseFile ofi
                 f2 <- parseFile dfi
                 return $ Move ((r1, f1), (r2, f2))

parseMove _ = ParseFail
-- primary pawn
-- TODO

catMoves :: [ParserAction] -> [(Pos, Pos)]
catMoves =  foldl (\ acc pa -> case pa of { Move m -> m : acc; _ -> acc; }) []
