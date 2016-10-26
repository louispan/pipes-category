module Main where

import Control.Monad
import Control.Monad.Trans.Class
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import qualified Pipes.PipeC as PPC
import Control.Arrow

sig1 :: Monad m => P.Producer Int m ()
sig1 = do
  P.yield 1
  P.yield 2
  P.yield 3
  P.yield 4
  P.yield 5
  P.yield 6
  P.yield 7
  P.yield 8
  P.yield 9

sig2 :: Monad m => P.Producer (Either Int String) m ()
sig2 = do
  P.yield $ Left 1
  P.yield $ Right "a"
  P.yield $ Left 2
  P.yield $ Right "b"
  P.yield $ Left 3
  P.yield $ Right "c"
  P.yield $ Left 4
  P.yield $ Right "d"
  P.yield $ Left 5
  P.yield $ Right "e"

sigConsumer :: Show a => P.Consumer a IO ()
sigConsumer = forever $ do
  a <- P.await
  lift $ print a

main :: IO ()
main = do
  putStrLn "\nOriginal"
  P.runEffect $ sig1 P.>-> sigConsumer

  putStrLn "\nfirst"
  P.runEffect $ sig1
    P.>-> PP.map (\a -> (a, a))
    P.>-> (PPC.getPipeC $ first $ PPC.PipeC (PP.map (+ 10)))
    -- P.>-> (_Unwrapping PPC.PipeC %~ first $ PP.map (+ 10))
    P.>-> sigConsumer

  putStrLn "\nsecond"
  P.runEffect $ sig1
    P.>-> PP.map (\a -> (a, a))
    P.>-> (PPC.getPipeC $ second $ PPC.PipeC (PP.map (+ 10)))
    -- P.>-> (_Unwrapping PPC.PipeC %~ second $ PP.map (+ 10))
    P.>-> sigConsumer

  putStrLn "\n***"
  P.runEffect $ sig1
    P.>-> PP.map (\a -> (a, a))
    P.>-> (PPC.getPipeC $ PPC.PipeC (PP.map (+ 10)) *** PPC.PipeC (PP.map (+ 20)))
    P.>-> sigConsumer

  putStrLn "\n&&&"
  P.runEffect $ sig1
    P.>-> (PPC.getPipeC $ PPC.PipeC (PP.map (+ 10)) &&& PPC.PipeC (PP.map (+ 20)))
    P.>-> sigConsumer

  putStrLn "\nOriginal2"
  P.runEffect $ sig2 P.>-> sigConsumer

  putStrLn "\nleft"
  P.runEffect $ sig2
    P.>-> (PPC.getPipeC $ left $ PPC.PipeC (PP.map (+ 10)))
    P.>-> sigConsumer

  putStrLn "\nright"
  P.runEffect $ sig2
    P.>-> (PPC.getPipeC $ right $ PPC.PipeC (PP.map (++ "!")))
    P.>-> sigConsumer

  putStrLn "\n+++"
  P.runEffect $ sig2
    P.>-> (PPC.getPipeC $ PPC.PipeC (PP.map (+ 10)) +++ PPC.PipeC (PP.map (++ "!")))
    P.>-> sigConsumer

  putStrLn "\n|||"
  P.runEffect $ sig2
    P.>-> (PPC.getPipeC $ PPC.PipeC (PP.map (show . (+ 10))) ||| PPC.PipeC (PP.map (++ "!")))
    P.>-> sigConsumer
