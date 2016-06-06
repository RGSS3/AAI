{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}

module System.Console.CmdArgs.AAI.CanMarshall
    ( CanMarshall (..)
    ) where

import Data.CanDefault

class (CanDefault a) => CanMarshall a where
  marshall :: String -> Maybe a

instance CanMarshall String where
  marshall = pure
