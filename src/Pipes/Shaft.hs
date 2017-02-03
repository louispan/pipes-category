{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Allows instances for Category, Arrow and ArrowChoice for 'Pipes.Pipe' using newtype wrapper 'Shaft'.
module Pipes.Shaft (Shaft(..)) where

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

newtype Shaft r m b c = Shaft { runShaft :: P.Pipe b c m r }

makeWrapped ''Shaft

instance Monad m => Category (Shaft r m) where
  id = Shaft P.cat
  {-# INLINABLE id #-}

  (Shaft a) . (Shaft b) = Shaft (b P.>-> a)
  {-# INLINABLE (.) #-}

instance Monad m => Arrow (Shaft r m) where
  arr f = Shaft (PE.arr f)
  {-# INLINABLE arr #-}

  -- | Send the first component of the input through the argument arrow, and copy the rest unchanged to the output.
  -- first :: Monad m => Shaft r m b c -> Shaft r m (b, d) (c, d)
  first (Shaft bc) = Shaft $ PL.evalStateP Nothing go -- State :: Maybe d
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
  {-# INLINABLE first #-}

  -- | A mirror image of 'first'.
  -- second :: Monad m => Shaft r m b c -> Shaft r m (d, b) (d, c)
  -- Don't use *** as an optimization, as it will require running first twice.
  second (Shaft bc) = Shaft $ PP.map swap
    P.>-> runShaft (first $ Shaft bc)
    -- P.>->  (_Unwrapping Shaft %~ first $ bc)
    P.>-> PP.map swap
  {-# INLINABLE second #-}

  -- Note: The following works, but may not actually be more optimal than default (***)
  -- So maybe it's better to reduce the amount of code to maintain.
  -- -- |Split the input between the two argument pipes and combine their output.
  -- -- (***) :: Monad m => Shaft r m b c -> Shaft r m b' c' -> Shaft r m (b, b') (c, c')
  -- (Shaft bc) *** (Shaft bc') = Shaft $ PL.evalStateP Nothing go -- State :: Maybe (Either c b')
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

instance Monad m => ArrowChoice (Shaft r m) where
  left (Shaft a) = Shaft (PE.left a)
  {-# INLINABLE left #-}

  right (Shaft a) = Shaft (PE.right a)
  {-# INLINABLE right #-}

  (Shaft a) +++ (Shaft b) = Shaft (a PE.+++ b)
  {-# INLINABLE (+++) #-}
