{-# LANGUAGE FlexibleInstances    #-}

module Data.CanDefault
    ( CanDefault (..)
    ) where

import           Control.Applicative (Alternative (empty))
import           Control.Monad       (MonadPlus (mzero))


class CanDefault d where
  def :: d


instance (Alternative d) => CanDefault (d a) where
  def = empty
