module Main (main) where

import           Common              (Game, Move, runEngine, startGame)
import           Control.Monad.State (execState)
import qualified Engine              as E
import           Parser              (ParserAction (Move, ParseFail, Resign),
                                      catMoves, parseMove)
import qualified ReaderEngine        as RE

main :: IO ()
main = do
    -- runEngineCLI startGame
    g1 <- runEngineFile E.move "games/enpassant.txt"
    g2 <- runEngineFile RE.move "games/enpassant.txt"
    print $ g1 == g2

runEngineCLI :: Game -> Move -> IO ()
runEngineCLI game move = do
    print game
    parseResult <- parseMove <$> getLine
    case parseResult of
            Resign    -> print "Exiting!"
            ParseFail -> do print "Invalid!"; runEngineCLI game move
            Move m    -> runEngineCLI (move game m) move

runEngineFile :: Move -> String -> IO Game
runEngineFile move filename = do
    moves <- reverse . catMoves . takeWhile (/= Resign) . map parseMove . lines <$> readFile filename
    return $ runEngine move startGame moves
