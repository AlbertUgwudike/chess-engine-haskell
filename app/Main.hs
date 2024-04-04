module Main (main) where

import           ChessData           (MoveF)
import           Common              (Game, runEngine, startGame)
import           Control.Monad.State (execState)
import qualified EngineFunctional    as EF
import qualified EngineMonadic       as EM
import           Parser              (ParserAction (Move, ParseFail, Resign),
                                      catMoves, parseMove)

main :: IO ()
main = do
    -- runEngineCLI startGame
    g1 <- runEngineFile EF.move "games/enpassant.txt"
    g2 <- runEngineFile EM.move "games/enpassant.txt"
    print $ g1 == g2

runEngineCLI :: Game -> MoveF -> IO ()
runEngineCLI game move = do
    print game
    parseResult <- parseMove <$> getLine
    case parseResult of
        Resign    -> print "Exiting!"
        ParseFail -> do print "Invalid!"; runEngineCLI game move
        Move m    -> runEngineCLI (move game m) move

runEngineFile :: MoveF -> String -> IO Game
runEngineFile move filename = do
    moves <- reverse . catMoves . takeWhile (/= Resign) . map parseMove . lines <$> readFile filename
    return $ runEngine move startGame moves
