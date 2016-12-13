module Utils ( defaultDelay
             , procToChan
             , spacer
             ) where

import System.IO
import Text.Printf (printf)
import Control.Concurrent (
  forkIO, threadDelay,
  Chan, writeChan, writeList2Chan, newChan)
import System.Process (
  StdStream(CreatePipe), std_out, createProcess, proc, shell,
  system)
import Control.Monad (forever, void)
import System.Environment (getEnv)

import Color

defaultDelay :: Double
defaultDelay = 1

procToHandle (cmd:args) = do
  (_,Just out,_,_) <- createProcess (proc cmd args) {std_out=CreatePipe}
  return out

procToChan cmdarr = do
  out <- procToHandle cmdarr
  hSetBuffering out LineBuffering
  chan <- newChan
  forkIO . forever $ writeChan chan =<< hGetLine out
  return chan

spacer :: String -> String
spacer s = " "++s++" "
