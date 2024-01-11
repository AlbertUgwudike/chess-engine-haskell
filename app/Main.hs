module Main (main) where

import Data.Char
import Parser
import ChessData
import Game

main :: IO ()
main = runEngineCLI startGame

runEngineCLI :: Game -> IO ()
runEngineCLI game = do
    print game
    parseResult <- parseMove <$> getLine 
    case parseResult of
        Resign -> print "Exiting!"
        ParseFail -> do { print "Invalid!"; runEngineCLI game; }
        Move m -> runEngineCLI $ move game m

runEngineFile :: String -> IO ()
runEngineFile filename = do
    moves <- reverse . catMoves . takeWhile (/= Resign) . map parseMove . lines <$> readFile filename
    print $ runEngine startGame moves 