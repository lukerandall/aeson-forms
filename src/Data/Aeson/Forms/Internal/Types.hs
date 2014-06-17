{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Forms.Internal.Types
    (
    -- * Types
      Form (..)
    , Result (..)
    , Errors (..)
    , Field
    ) where

import           Control.Applicative
import           Data.Aeson (Value (..), ToJSON (..), object, (.=))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid
import           Data.Text (Text)


------------------------------------------------------------------------------
-- | 'Form' represents a computation that when given a JSON 'Value' will
-- yield either 'Success' with the parsed data value or 'Failed' with
-- validation errors.
data Form m a where
    Form :: Monad m => (Maybe Value -> m (Result a)) -> Form m a


------------------------------------------------------------------------------
instance Functor (Form m) where
    fmap f (Form action) = Form $ \json -> do
        g <- action json
        return $ fmap f g


------------------------------------------------------------------------------
instance Monad m => Applicative (Form m) where
    pure x = Form $ \_ -> return $ Success x
    Form f <*> Form g = Form $ \json -> do
        f' <- f json
        g' <- g json
        return $ f' <*> g'


------------------------------------------------------------------------------
instance Monad m => Alternative (Form m) where
    empty = Form $ \_ -> return empty
    Form f <|> Form g = Form $ \json -> do
       f' <- f json
       g' <- g json
       return $ f' <|> g'


------------------------------------------------------------------------------
-- | The result of running a 'Form'. If parsing is successful, Success is
-- returned along with the parsed value, else Failed is returned with a
-- HashMap of validation Errors.
data Result a =
      Success a
    | Failed Errors
      deriving (Show, Eq)


------------------------------------------------------------------------------
instance Functor Result where
    fmap f (Success a) = Success $ f a
    fmap _ (Failed a) = Failed a


------------------------------------------------------------------------------
instance Applicative Result where
    pure = Success
    Success a <*> Success b = Success (a b)
    Success _ <*> Failed b   = Failed b
    Failed a   <*> Success _ = Failed a
    Failed a   <*> Failed b   = Failed (a <> b)


------------------------------------------------------------------------------
instance Alternative Result where
    empty = Failed . Errors $ HashMap.empty
    Success a <|> _ = Success a
    Failed _  <|> b = b


------------------------------------------------------------------------------
instance Monad Result where
    return = pure
    Failed x  >>= _ = Failed x
    Success x >>= f = f x


------------------------------------------------------------------------------
-- | Validation errors, keyed by 'Field' name.
newtype Errors = Errors (HashMap Field [Text]) deriving (Show, Eq)


------------------------------------------------------------------------------
instance Monoid Errors where
    mempty = Errors HashMap.empty
    Errors a `mappend` Errors b = Errors (a `combine` b)
      where
        combine = HashMap.unionWith (++)


------------------------------------------------------------------------------
instance ToJSON Errors where
    toJSON (Errors errors) = object ["errors" .= toJSON errors]


------------------------------------------------------------------------------
-- | The name of a JSON field.
type Field = Text
