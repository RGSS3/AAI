{-# LANGUAGE FlexibleInstances    #-}

module Data.CanDefault
    ( CanDefault (..)
    ) where

import           Control.Applicative (Alternative (empty))
import           Control.Monad       (MonadPlus (mzero))


class CanDefault d where
  def :: d

instance CanDefault () where
  def = ()

instance CanDefault [a] where
  def = []

instance CanDefault (IO ()) where
  def = return ()
