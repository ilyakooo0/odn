module Main (main) where

import Colourista
import Control.Concurrent
import Control.Monad
import Data.Functor
import qualified Data.Text as T
import System.Environment
import System.Exit
import System.IO
import System.Process.Typed

main :: IO ()
main = getArgs >>= runPrograms

runPrograms :: [String] -> IO ()
runPrograms progs = do
  exited <- newEmptyMVar
  processes <- forM (zip progs (prettifyProcesses progs)) $ \(prog, pretty) -> do
    p <-
      startProcess . setStderr createPipe
        . setStdout createPipe
        . setStdin nullStream
        $ shell prog
    void . forkIO $ do
      e <- waitExitCode p
      putMVar exited (pretty, e)
    return (pretty, p)

  forever $ do
    isEmpty <- isEmptyMVar exited
    let writeLogs =
          forM (zip colors processes) $ \(color, (prog, p)) -> do
            (||)
              <$> writeOutput [color] prog (getStdout p)
              <*> writeOutput [color] prog (getStderr p)

    if isEmpty
      then do
        written <- writeLogs
        if or written
          then return ()
          else threadDelay 100000
      else do
        (prog, exitCode) <- readMVar exited
        infoMessage $ T.pack prog <> " exited with " <> (T.pack . show) exitCode
        forM_ processes (stopProcess . snd)
        exitWith exitCode

colors :: [String]
colors = cycle [green, blue, magenta, cyan]

writeOutput :: [String] -> String -> Handle -> IO Bool
writeOutput color prog h = do
  inputAvailable <- hReady h
  if inputAvailable
    then do
      l <- hGetLine h
      putStrLn $ formatWith color (prog <> " | ") <> l
      return True
    else return False

prettifyProcesses :: [String] -> [String]
prettifyProcesses ps' =
  let ps = take 50 <$> ps'
      maxLen = maximum . fmap length $ ps
   in ps <&> \p -> p <> replicate (maxLen - length p) ' '
