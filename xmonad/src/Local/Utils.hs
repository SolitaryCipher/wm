module Local.Utils ( defaultDelay
                   , procToChan
                   , spacer
                   ) where

import System.IO
--import Text.Printf (printf)
import Control.Concurrent (
  forkIO, Chan, writeChan, newChan)
import System.Process (
  StdStream(CreatePipe), std_out, createProcess, proc)
import Control.Monad (forever)


defaultDelay :: Double
defaultDelay = 1

procToHandle :: [FilePath] -> IO Handle
procToHandle (cmd:args) = do
  (_,Just out,_,_) <- createProcess (proc cmd args) {std_out=CreatePipe}
  return out
procToHandle _ = return stdout

procToChan :: [FilePath] -> IO (Chan String)
procToChan cmdarr = do
  out <- procToHandle cmdarr
  hSetBuffering out LineBuffering
  chan <- newChan
  _ <- forkIO . forever $ writeChan chan =<< hGetLine out
  return chan

spacer :: String -> String
spacer s = " "++s++" "
