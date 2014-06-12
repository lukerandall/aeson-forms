{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Forms.Testbed
    ( User (..)
    , Person (..)
    , A.decode
    , values
    , module Data.Aeson.Forms.Combinators
    , module Control.Applicative
    ) where

import           Control.Applicative
import qualified Data.Aeson as A
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Data.ByteString.Lazy.Char8 as BS
import           Data.Aeson.Forms.Combinators

data User = User Text Text deriving (Show)

data Person = Person Text Text (Maybe Text) Bool deriving (Show)

js :: BS.ByteString
js = BS.pack "{\"name\":\"Luke\",\"last_name\":\"Randall\",\"nickname\":\"ldrndll\",\"age\":29,\"is_client\":true}"

values :: Maybe A.Value
values = A.decode js
