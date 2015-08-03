{- |
Module      :  $Header$
Description :  A simple type class for routing arguments to commands.
Author      :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License     :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

A simple type class implementing a simple routing table for commands.
-}
module Control.Router
( Router (..)
) where

import Control.Command


class (Command a) => Router a where
    routes :: a -> String -> Bool

