{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.CanDefault
    ( CanDefault (..)
    ) where

import           Control.Applicative (Alternative (empty))
import           Control.Monad       (MonadPlus (mzero))


class CanDefault d where
  def :: d


instance (Num a) => CanDefault a where
  def = 0

instance (Alternative d) => CanDefault (d a) where
  def = empty
