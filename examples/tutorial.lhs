> {-# LANGUAGE OverloadedStrings #-}

JSON field names use Text, so this makes it easier to create fields.

> import Control.Applicative ((<$>), (<*>))
> import Data.Aeson.Forms
> import Data.Text (Text)

We'll use Form's applicative interface to build up our form, using
functions from Data.Aeson.Forms.

> data Person = Person
>     { name :: Text
>     , lastName :: Text
>     , nickname :: Maybe Text
>     , client :: Bool
>     } deriving (Show)

Here we define a basic data type. We'll use this to demonstrate how to use
the various functions Data.Aeson.Forms provides. Let's create a form for
our Person:

> personForm :: Monad m => Form m Person
> personForm = Person
>     <$> "name"      .:  text
>     <*> "last_name" .:  text
>     <*> "nickname"  .:? opt text
>     <*> "is_client" .:  bool

Our Person Form gives us a good idea of how our applicative interface works.
We call the .: function to extract a field, providing it the name of the
field to extract from the JSON. Using functions like text and bool, we're
able to convert the JSON values to Haskell data types.

By default, fields you lookup are required to be present. If they are not,
the form returns an Errors with messages for each failed lookup. Nickname
however is an optional value, so we use .:? to look it up and return
Maybe depending on whether the field is present or not. Additionally, using
opt we can turn a validator - like text or bool - that fails if it is not
given a value to one that only runs if it is given a value.

Let's try running our form against some actual data. For this, we use
runForm, which takes an optional JSON value and a Form to run it against.
The value is optional as the JSON might fail to decode, or the form might
not require any values to be provided. Ours does though, so let's see what
happens when we give it Nothing

> runForm Nothing personForm

You should see a number of validation errors:

Failed (Errors fromList [("is_client",["Is required"]),("name",["Is required"]),("last_name",["Is required"])])
