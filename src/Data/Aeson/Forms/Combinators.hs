{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Forms.Combinators
    (
    -- * Forms
      runForm
    , Form (..)
    -- * Form fields
    , (.:)
    , (.:?)
    , (.:!?)
    -- * Validators
    , text
    , string
    -- * Lower level helpers
    , success
    , failed
    , errors
    ) where

import           Control.Applicative
import           Control.Lens.Fold ((^?))
import           Data.Aeson (Value (..))
import           Data.Aeson.Lens (key)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Data.Aeson.Forms

data Form m a where
    Form :: Monad m => (Value -> m (Result a)) -> Form m a

runForm :: Value -> Form m a -> m (Result a)
runForm json (Form f) = f json


instance Functor (Form m) where
    fmap f (Form form) = Form go
      where
        go json = do
            g <- form json
            return $ fmap f g

instance Monad m => Applicative (Form m) where
    pure x = Form $ \_ -> success x
    Form f <*> Form g = Form go
      where
        go json = do
            f' <- f json
            g' <- g json
            return $ f' <*> g'


text :: Monad m => Field -> Form m Text
text field = Form go
  where
    go (String t) = success t
    go _          = failed $ errors field ["Must be of type string"]

string :: Monad m => Field -> Form m String
string field = Form go
  where
    go (String t) = success $ show t
    go _          = failed $ errors field ["Must be of type string"]

(.:) :: Monad m => Field -> (Field -> Form m a) -> Form m a
(.:) field validate = go
  where
    go :: Form m a
    go json = case json ^? key field of
        Just value -> validate field value
        Nothing    -> Form $ failed missing
    missing = errors field ["Is required"]


(!!) :: Monad m => Value -> Field -> (Field -> Value -> m (Result a)) -> m (Result a)
(!!) obj field validate =
    case obj ^? key field of
        Just value -> validate field value
        Nothing    -> failed missing
  where
    missing = errors field ["Is required"]

(.:?) :: Monad m => Value -> Field -> (Field -> Maybe Value -> m (Result (Maybe a))) -> m (Result (Maybe a))
(.:?) obj field validate = validate field valueM
  where
    valueM = obj ^? key field

(.:!?) :: Monad m => Value -> Field -> (Field -> Maybe Value -> m (Result a)) -> m (Result a)
(.:!?) obj field validate = validate field valueM
  where
    valueM = obj ^? key field

success :: Monad m => a -> m (Result a)
success = return . Success

failed :: Monad m => Errors Text -> m (Result a)
failed = return . Error

errors :: Field -> [a] -> Errors a
errors field errs = Errors $ HashMap.singleton field errs
