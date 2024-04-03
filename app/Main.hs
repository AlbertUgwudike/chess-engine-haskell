module Main (main) where

import           ChessData
import           Common              (activeKingPos, mateGame, startGame)
import           Control.Monad.State (evalState, execState, runState)
import qualified Engine              as E
import           Parser              (ParserAction (Move, ParseFail, Resign),
                                      catMoves, parseMove)
import qualified ReaderEngine        as RE

main :: IO ()
main = do
    runEngineCLI startGame
    runEngineFile "games/enpassant.txt"

runEngineCLI :: Game -> IO ()
runEngineCLI game = do
    print game
    parseResult <- parseMove <$> getLine
    case parseResult of
            Resign    -> print "Exiting!"
            ParseFail -> do print "Invalid!"; runEngineCLI game
            Move m    -> runEngineCLI $ execState (move m) game

runEngineFile :: String -> IO ()
runEngineFile filename = do
    moves <- reverse . catMoves . takeWhile (/= Resign) . map parseMove . lines <$> readFile filename
    print $ runEngine startGame moves
