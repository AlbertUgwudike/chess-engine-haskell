module Main (main) where

import           ChessData           (MoveF, Pos)
import           Common              (Game, runEngine, startGame)
import           Control.Monad       (when)
import           Control.Monad.State (execState)
import           Data.List           (intersperse)
import qualified EngineFunctional    as EF
import qualified EngineMonadic       as EM
import           Parser              (ParserAction (Move, ParseFail, Resign),
                                      catMoves, parseMove, writeMove)
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure)

main :: IO ()
main = do
    -- args <- getArgs
    -- when (null args) (print "provide output file!" >> exitFailure)
    -- moves <- runEngineCLI EM.move startGame
    -- writeFile "out.txt" . unlines $ map writeMove moves
    g1 <- runEngineFile EF.move "games/whitemate.txt"
    g2 <- runEngineFile EM.move "games/whitemate.txt"
    when (g1 == g2) $ print g1

runEngineCLI :: MoveF -> Game -> IO [(Pos, Pos)]
runEngineCLI move game = do
    print game
    parseResult <- parseMove <$> getLine
    case parseResult of
        Resign    -> do print "Exiting!"; return []
        ParseFail -> do print "Invalid!"; runEngineCLI move game
        Move m    -> (m:) <$> runEngineCLI move (move game m)

runEngineFile :: MoveF -> String -> IO Game
runEngineFile move filename = do
    moves <- reverse . catMoves . takeWhile (/= Resign) . map parseMove . lines <$> readFile filename
    return $ runEngine move startGame moves
