{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Allows instances for Category, Arrow and ArrowChoice for 'Pipes.Pipe' using newtypewrapper 'PipeC'.
module Pipes.PipeC (PipeC(..)) where

import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Tuple
import qualified Pipes as P
import qualified Pipes.Extras as PE
import qualified Pipes.Lift as PL
import qualified Pipes.Prelude as PP

newtype PipeC m r b c = PipeC { getPipeC :: P.Pipe b c m r }

makeWrapped ''PipeC

instance Monad m => Category (PipeC m r) where
  id = PipeC P.cat
  (PipeC a) . (PipeC b) = PipeC (b P.>-> a)

instance Monad m => Arrow (PipeC m r) where
  arr f = PipeC (PE.arr f)

  -- | Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.
  -- first :: Monad m => PipeC m r b c -> PipeC m r (b, d) (c, d)
  first (PipeC bc) = PipeC $ PL.evalStateP Nothing go -- State :: Maybe d
   -- Not reusing (***) as this requires a simpler state
   where

    justd (_, d) = Just d

    go = store' justd -- store (Just d)
      P.>-> PP.map fst -- get b from (b, d)
      P.>-> P.hoist lift bc -- make original pipe bc stateful
      P.>-> outcd

    outcd = forever $ do
      c <- P.await -- c from output of pipe bc
      s <- get -- (Just d) from storage
      case s of
        Just d -> P.yield (c, d)
        Nothing -> pure () -- shouldn't happen

    -- | Store the output of the pipe into a MonadState.
    store' :: MonadState s m => (a -> s) -> P.Pipe a a m r
    store' f = forever $ do
      a <- P.await
      put $ f a
      P.yield a

  -- | A mirror image of 'first'.
  -- second :: Monad m => PipeC m r b c -> PipeC m r (d, b) (d, c)
  -- Don't use *** as an optimization, as it will require running first twice.
  second (PipeC bc) = PipeC $ PP.map swap
    P.>-> getPipeC (first $ PipeC bc)
    -- P.>->  (_Unwrapping PipeC %~ first $ bc)
    P.>-> PP.map swap

  -- Note: The following works, but may not actually be more optimal than default (***)
  -- So maybe it's better to reduce the amount of code to maintain.
  -- -- |Split the input between the two argument pipes and combine their output.
  -- -- (***) :: Monad m => PipeC m r b c -> PipeC m r b' c' -> PipeC m r (b, b') (c, c')
  -- (PipeC bc) *** (PipeC bc') = PipeC $ PL.evalStateP Nothing go -- State :: Maybe (Either c b')
  --  where
  --   justb' (_, b') = Just (Right b')

  --   -- store (Just (Left c)), and yields b'
  --   storec = forever $ do
  --     c <- P.await -- c from output of pipe bc
  --     s <- get -- Just (Right b') from storage
  --     put $ Just (Left c) -- store (Just (Left c))
  --     case s of
  --       Just (Right b') -> P.yield b'
  --       _ -> pure () -- shouldn't happen

  --   -- get (Just (Left c)) from stroage, and combine with awaited c'
  --   outcc' = forever $ do
  --     c' <- P.await -- c' from output of pipe bc'
  --     s <- get -- Just (Left c) from storage
  --     case s of
  --       Just (Left c) -> P.yield (c, c')
  --       _ -> pure () -- shouldn't happen

  --   go = store' justb' -- store (Just (Right b'))
  --     P.>-> PP.map fst -- get b from (b, b')
  --     P.>-> P.hoist lift bc -- make original pipe bc stateful
  --     P.>-> storec -- store (Just (Left c)), and yields b'
  --     P.>-> P.hoist lift bc' -- make original pipe bc' stateful
  --     P.>-> outcc'

instance Monad m => ArrowChoice (PipeC m r) where
  left (PipeC a) = PipeC (PE.left a)
  right (PipeC a) = PipeC (PE.right a)
  (PipeC a) +++ (PipeC b) = PipeC (a PE.+++ b)
