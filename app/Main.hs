module Main where

import System.Environment
import Lib

main :: IO ()
main = do
    arg <- head <$> getArgs
    let parseRes = parseCalcAST mainParser arg
    case parseRes of
        Right r -> do
            putStrLn $ "calculateAST from\n" ++ show r
            let (Right calcRes) = fromRational <$> (calc r)
            putStrLn $ "to " ++ show calcRes
        Left e -> print e
