{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.Forms
    ( Result (..)
    , Errors (..)
    , Field
    ) where

import           Control.Applicative
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid
import           Data.Text (Text)


type Field = Text

newtype Errors a = Errors (HashMap Field [a]) deriving (Show, Eq)

instance Monoid (Errors a) where
    mempty = Errors HashMap.empty
    Errors a `mappend` Errors b = Errors (a `combine` b)
      where
        combine = HashMap.unionWith (++)

data Result a =
      Error (Errors Text)
    | Success a
      deriving (Show, Eq)

instance Functor Result where
    fmap f (Success a) = Success $ f a
    fmap _ (Error a) = Error a

instance Applicative Result where
    pure = Success
    Success a <*> Success b = Success (a b)
    Success _ <*> Error b   = Error b
    Error a   <*> Success _ = Error a
    Error a   <*> Error b   = Error (a <> b)

instance Monad Result where
    return = pure
    (Error x)   >>= _ = Error x
    (Success x) >>= f = f x
