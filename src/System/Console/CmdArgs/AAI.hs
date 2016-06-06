{-# LANGUAGE FlexibleInstances #-}

module System.Console.CmdArgs.AAI
    ( Desc
    , AAI (..)

    , section
    , param
    , flag
    ) where

import           Control.Applicative
import           Control.Monad                          (join)

import           Data.CanDefault
import           Data.Foldable                          (asum)

import           System.Console.CmdArgs.AAI.CanMarshall


type Desc = Int -> String


instance Show (Int -> String) where
  show f = show $ f 0


newtype AAI a = AAI
  { runAAI :: [String] -> [(a,Int,Desc,[String],[String])]
  }


section :: (CanDefault d) => String -> [AAI d] -> AAI d
section n ds = AAI $ \as -> case as of
  a:as' ->
    if   a == n
    then map (\(x,i,d,pi,rs) -> (x,i,d,a:pi,rs)) (asum $ map (`runAAI` as') ds)
    else map (\(x,i,d,pi,rs) -> (x,i+5,d,a:pi,rs)) (asum $
           map (`runAAI` as) ds ++ map (`runAAI` as') ds)
  _ -> [(def,10,const "",[n],[])]

param :: (Show a,CanDefault a,CanMarshall a) => AAI a
param = AAI $ \as -> case as of
  a:as' -> case marshall a of
    Just a' -> [(a',0,const "",[a],as')]
    _       -> let a = def in [(a,10,const "",[show a],as')]
  _ -> let a = def in [(a,10,const "",[show a],[])]

flag :: String -> AAI ()
flag f = AAI $ \as -> case as of
  a:as' ->
    if   a == f
    then [((),0,const "",[a],as')]
    else [((),5,const "",[a],as')]
  _ -> [((),10,const "",[f],[])]


instance Functor AAI where
  fmap f a = AAI $ \as -> map (\(a,i,d,pi,as) -> (f a,i,d,pi,as)) (runAAI a as)

instance Applicative AAI where
  pure a = AAI $ \as -> [(a,0,const "",[],as)]

  f <*> a = AAI $ \as ->
    asum $ map (\(g,i,d,pi,as') ->
      map (\(a,i',d',pi',as'') ->
        (g a,i+i',\i -> d i ++ d' i,pi ++ pi',as''))
        (runAAI a as')) (runAAI f as)

instance Alternative AAI where
  l <|> r = AAI $ \as -> runAAI l as <|> runAAI r as

  empty = AAI $ const empty
