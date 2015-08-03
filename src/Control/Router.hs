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
, route
) where

import Control.Applicative
import Control.Command
import Control.Monad

-- | A type class describing basing routing table.
class (Command a) => Router a where
    -- | Checks if a command is routable to a specific command name.
    routes :: a -> String -> Bool

-- | Route a specific instruction to a command.
route :: (Command a, Router a, Alternative m, Monad m) => [a] -> String -> m a
route []     _   =
    empty
route (x:xs) cmd =
    if routes x cmd
       then return x
       else route xs cmd

