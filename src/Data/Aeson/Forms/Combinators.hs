{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Forms.Combinators
    (
    -- * Working with forms
      runForm
    , withForm
    , runAction
    -- * Form fields
    , (.:)
    , (.:!)
    , (.:?)
    -- * Validators
    , text
    , string
    , opt
    -- * Lower level helpers
    , success
    , failed
    , errors
    ) where

import           Control.Applicative ((<$>))
import           Control.Lens.Fold ((^?))
import           Data.Aeson (Value (..))
import           Data.Aeson.Lens (key)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Data.Aeson.Forms.Internal.Types


------------------------------------------------------------------------------
-- | Runs the form against the provided JSON 'Value'.
runForm :: Maybe Value -> Form m a -> m (Result a)
runForm json (Form f) = f json

------------------------------------------------------------------------------
-- | Takes a function that extracts a field from a JSON 'Value' and turns it
-- into a 'Form'.
withForm :: Monad m => (Maybe Value -> m (Result a)) -> Form m a
withForm = Form


------------------------------------------------------------------------------
-- | Runs the action from a form against the given JSON 'Value', yielding a
-- result.
runAction :: Form m a -> Maybe Value -> m (Result a)
runAction (Form action) = action


------------------------------------------------------------------------------
-- | Extracts a field of type 'Text'. If field is not a string validation
-- fails with the message "Must be of type string". See also `string`.
text :: Monad m
     => Field        -- ^ The name of the field to extract
     -> Form m Text
text field = withForm go
  where
    go (Just (String t)) = success t
    go _          = failed $ errors field ["Must be of type string"]


------------------------------------------------------------------------------
-- | Extracts a field of type 'String'. If field is not a string validation
-- fails with the message "Must be of type string". See also `text`.
string :: Monad m
       => Field          -- ^ The name of the field to extract
       -> Form m String
string field = withForm go
  where
    go (Just (String t)) = success $ show t
    go _          = failed $ errors field ["Must be of type string"]


------------------------------------------------------------------------------
-- | Converts a validator that returns 'a' into one that returns 'Maybe' a. If
-- the value is not present 'Nothing' will be returned.
opt :: Monad m
    => (Field -> Form m a)  -- ^ The validator to run if field is present
    -> Field                -- ^ The name of the field to extract
    -> Form m (Maybe a)
opt validate field = withForm go
  where
    go json@(Just _) = do
        result <- runAction (validate field) json
        return $ Just <$> result
    go (Nothing) = success Nothing


------------------------------------------------------------------------------
-- | Extracts a required field and runs the given validator against it. If the
-- field is absent validation fails with the message "Is required".
(.:) :: Monad m
     => Field                -- ^ The name of the field to extract
     -> (Field -> Form m a)  -- ^ Validation function to run against the field
     -> Form m a
(.:) field validate = withForm go
  where
    go (Just json) = case json ^? key field of
        Just value -> runAction (validate field) $ Just value
        Nothing    -> missing
    go Nothing     = missing
    missing = failed $ errors field ["Is required"]
infixr 5 .:

------------------------------------------------------------------------------
-- | Extracts a field and runs the given validator against it. The validator
-- is responsible for handling the field being 'Nothing'.
(.:!) :: Monad m
      => Field                -- ^ The name of the field to extract
      -> (Field -> Form m a)  -- ^ Validation function to run against the field
      -> Form m a
(.:!) field validate = withForm go
  where
    go (Just json) = go' $ json ^? key field
    go Nothing     = go' Nothing
    go' = runAction (validate field)
infixr 5 .:!


------------------------------------------------------------------------------
-- | Extracts a field and runs the given validator against it. If the field is
-- absent it returns nothing.
(.:?) :: Monad m
      => Field                        -- ^ The name of the field to extract
      -> (Field -> Form m (Maybe a))  -- ^ Validation function to run against the field
      -> Form m (Maybe a)
(.:?) field validate = withForm go
  where
    go (Just json)
      | Just value <- json ^? key field = runAction (validate field) (Just value)
    go _     = success Nothing
infixr 5 .:?


------------------------------------------------------------------------------
-- | Return parsed value from a successful parse.
success :: Monad m => a -> m (Result a)
success = return . Success

------------------------------------------------------------------------------
-- | Return validation errors for a failed parse.
failed :: Monad m => Errors -> m (Result a)
failed = return . Failed

------------------------------------------------------------------------------
-- | Return 'Errors' for the given field and validation error messages.
errors  :: Field   -- ^ Field that failed validation
        -> [Text]  -- ^ List of validation error messages
        -> Errors
errors field errs = Errors $ HashMap.singleton field errs
