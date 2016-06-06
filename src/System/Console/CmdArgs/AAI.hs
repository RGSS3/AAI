{-# LANGUAGE FlexibleInstances #-}

module System.Console.CmdArgs.AAI
    ( Desc
    , AAI (..)

    , aai

    , branch
    , param
    , flag
    , section
    ) where

import           Control.Applicative
import           Control.Monad                          (join)

import           Data.CanDefault
import           Data.Foldable                          (asum)
import           Data.List                              (nubBy)

import           System.Console.CmdArgs.AAI.CanMarshall
import           System.Environment                     (getArgs)
import           System.IO


type Desc = Int -> String


instance Show (Int -> String) where
  show f = show $ f 0


newtype AAI a = AAI
  { runAAI :: [String] -> [(a,Int,Desc,[String],[String])]
  }


noValidMatch :: [String] -> [(IO (),Int,Desc,[String],[String])] -> IO ()
noValidMatch args rs = do
  putStrLn $ ">> " ++ unwords args ++ " <<"
  putStrLn "Did you mean one of these?"
  let for i rs = case rs of
        (_,_,d,pi,_):rs -> do
          putStrLn $ "[" ++ show i ++ "] " ++ unwords pi
          for (i + 1) rs
        _ -> putStrLn ""
  for 0 $ take 10 $ nubBy (\(_,_,_,l,_) (_,_,_,r,_) -> l == r) rs


ambiguousMatch :: [String] -> [(IO (),Int,Desc,[String],[String])] -> IO ()
ambiguousMatch args rs = do
  putStrLn $ ">> " ++ unwords args ++ " <<"
  putStrLn "is ambiguos; it could mean the fhe following:"
  let for rs = case rs of
        (_,_,d,_,_):rs -> putStrLn (d 0)
        _              -> putStrLn ""
  for rs

aai :: AAI (IO ()) -> [String] -> IO ()
aai a args = case args of
  "verbose":args ->
    let rs = runAAI a args
        ps = filter (\(_,i,_,_,_) -> i == 0) rs
     in case ps of
       [(a,_,_,_,_)] -> a
       []            -> noValidMatch args rs
       ps            -> ambiguousMatch args ps
  args -> do
    putStrLn $ ">> " ++ unwords args ++ " <<"
    let rs = runAAI a args
        ps = filter (\(_,i,_,_,_) -> i == 0) rs
     in case ps of
      [(a,_,_,_,_)] -> a
      []            -> putStrLn "couldn't be resolved."
      _             -> putStrLn "is ambiguous."
    putStrLn ""


branch :: [AAI d] -> AAI d
branch ds = AAI $ \as -> asum $ map (`runAAI` as) ds

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
    then [((),0,const "",[f],as')]
    else [((),5,const "",[f],as'),((),10,const "",[f],as)]
  _ -> [((),10,const "",[f],[])]

section :: String -> [AAI d] -> AAI d
section n ds = flag n *> branch ds


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
