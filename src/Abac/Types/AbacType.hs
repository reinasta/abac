{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Abac.Types.AbacType where

import Data.Either (isLeft)
import Control.Monad.Reader
import Abac.Types.ParserTypes
import Prelude hiding (Word)


-- types

type Abac r = Abc Err r

type InAbac r = IO (Either Err r)



newtype Abc e r = Abc { runAbc :: IO (Either e r) }

-- instances

instance Functor (Abc e) where
  fmap f (Abc ier) = Abc $ (fmap . fmap) f ier

instance (Functor (Abc e)) => Applicative (Abc e) where
  pure r = Abc $ return $ Right r
  mf <*> ma =
    let appio :: IO (Either e (a -> b)) -> IO (Either e a) -> IO (Either e b)
        appio ief iea = do
          ef <- ief
          ea <- iea
          return $ ef <*> ea
    in Abc $ runAbc mf `appio` runAbc ma

instance (Applicative (Abc e)) => Monad (Abc e) where
  return = pure
  ma >>= f =
    let bindio :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
        bindio iea f' = do
          ea <- iea
          case ea of
            Right r -> f' r
            Left e -> return $ Left e
    in Abc $ runAbc ma `bindio` (fmap runAbc f)

-- errors

-- does the Abac value encapsulate an error or a result?
isErr :: Abac r -> Abac Bool
isErr = isPred isLeft

isRes :: Abac r -> Abac Bool
isRes = isPred isLeft

isPred :: (Either Err r -> Bool) -> Abac r -> Abac Bool
isPred prd abc =
  let Abc ier = abc
      go :: IO (Either Err Bool)
      go = do
        er <- ier
        return $ Right $ prd er
  in  Abc go

-- add an Abac error
putError :: Err -> Abac a
putError err = Abc $ return $ Left err

--error type
data Err = Parsing Text
         | MiscErr Text
         | Signal Text
         deriving (Show,Eq)

