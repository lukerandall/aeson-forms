{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Forms.Testbed
    ( User (..)
    , Person (..)
    , success
    , A.decode
    , key
    , (^?)
    , values
    , (.:)
    , (.:?)
    , errors
    , text
    , string
    ) where

import           Control.Lens.Fold ((^?))
import qualified Data.Aeson as A
import           Data.Aeson.Lens (key)
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Data.ByteString.Lazy.Char8 as BS
import           Data.Aeson.Forms.Combinators

data User = User Text Text deriving (Show)

data Person = Person Text Text (Maybe Text) deriving (Show)

js :: BS.ByteString
js = BS.pack "{\"name\":\"Luke\",\"last_name\":\"Randall\",\"age\":29}"

values :: Maybe A.Value
values = A.decode js
