{- |
Module      :  $Header$
Description :  A command interface.
Author      :  Nils 'bash0r' Jonsson
Copyright   :  (c) 2015 Nils 'bash0r' Jonsson
License     :  MIT

Maintainer  :  aka.bash0r@gmail.com
Stability   :  unstable
Portability :  non-portable (Portability is untested.)

A command interface.
-}
module Control.Command
( Command (..)
) where


-- | A type class describing a basic command.
class Command a where
    -- | Execute a command.
    execute :: a -> [String] -> IO ()
    -- | Execute the help of a command.
    help    :: a -> String

