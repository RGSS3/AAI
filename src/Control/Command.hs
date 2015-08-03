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
( execute
, help
) where


class Command a where
    execute :: a -> [String] -> IO ()
    help    :: a -> [String] -> IO ()

