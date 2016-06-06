module System.Console.CmdArgs.AAI
    ( Desc
    , AAI (..)
    ) where

import           Control.Applicative
import           Control.Monad       (join)


type Desc = Int -> String


newtype AAI a = AAI
  { runAAI :: [String] -> [(a,Int,Desc,[String],[String])]
  }


instance Functor AAI where
  fmap f a = AAI $ \as -> map (\(a,i,d,pi,as) -> (f a,i,d,pi,as)) (runAAI a as)

instance Applicative AAI where
  pure a = AAI $ \as -> [(a,0,const "",[],as)]

  f <*> a = AAI $ \as ->
    join $ map (\(g,i,d,pi,as') ->
      map (\(a,i',d',pi',as'') ->
        (g a,i+i',\i -> d i ++ d' i,pi ++ pi',as''))
        (runAAI a as')) (runAAI f as)

instance Alternative AAI where
  l <|> r = AAI $ \as -> runAAI l as <|> runAAI r as

  empty = AAI $ const empty
