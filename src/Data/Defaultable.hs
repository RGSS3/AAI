{-# LANGUAGE FlexibleInstances #-}

module Data.Defaultable
    ( Defaultable (..)
    ) where

import           Control.Applicative (Alternative (empty))
import           Control.Monad       (MonadPlus (mzero))


class Defaultable d where
  def :: d


instance Defaultable Int where
  def = 0

instance (Alternative d) => Defaultable (d a) where
  def = empty
