{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Forms.Testbed
    ( User (..)
    , success
    , A.decode
    , key
    , (^?)
    , values
    , (.:)
    , (.:?)
    , (.:!?)
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

js :: BS.ByteString
js = BS.pack "{\"name\":\"Luke\",\"last_name\":\"Randall\"}"

values :: A.Value
values = fromJust $ A.decode js
  where
    fromJust (Just x) = x
    fromJust _        = error "fromJust: Nothing"
