{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Forms.Combinators
    (
    -- * Working with forms
      Form
    , runForm
    , withForm
    , runAction
    -- * Form fields
    , (.:)
    , (.:!)
    , (.:?)
    , (>->)
    -- * Validators
    , text
    , string
    , int
    , integer
    , integral
    , float
    , double
    , realFloat
    , bool
    , object
    , array
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
import           Data.Scientific (floatingOrInteger)
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Data.Aeson.Forms.Internal.Types


------------------------------------------------------------------------------
-- | Runs the form against the provided JSON 'Value'.
runForm :: Maybe Value -> Form m a -> m (Result a)
runForm json (Form action) = action json

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
-- | Combines a Form m a and a function from a -> b into a Form m b.
(>->) :: Monad m => Form m a -> (a -> b) -> Form m b
(>->) = flip (<$>)


------------------------------------------------------------------------------
-- | Extracts a field of type 'Text'. If field is not a string validation
-- fails with the message "Must be a string". See also `string`.
text :: Monad m
     => Field        -- ^ The name of the field to extract
     -> Form m Text
text field = withForm go
  where
    go (Just (String t)) = success t
    go _ = failed $ errors field ["Must be a string"]


------------------------------------------------------------------------------
-- | Extracts a field of type 'String'. If field is not a string validation
-- fails with the message "Must be a string". See also `text`.
string :: Monad m
       => Field          -- ^ The name of the field to extract
       -> Form m String
string field = withForm go
  where
    go (Just (String t)) = success $ show t
    go _ = failed $ errors field ["Must be a string"]


------------------------------------------------------------------------------
-- | Extracts a field of type 'Int'. If field is not an integer validation
-- fails with the message "Must be an integer". See also `integer` and
-- `integral`.
int :: Monad m
    => Field       -- ^ The name of the field to extract
    -> Form m Int
int = integral


------------------------------------------------------------------------------
-- | Extracts a field of type 'Integer'. If field is not an integer validation
-- fails with the message "Must be an integer". See also `int` and
-- `integral`.
integer :: Monad m
        => Field           -- ^ The name of the field to extract
        -> Form m Integer
integer = integral


------------------------------------------------------------------------------
-- | Extracts a field of type 'Integral i => i'. If field is not an integer
-- validation fails with the message "Must be an integer". See also `int`
-- and `integer`.
integral :: (Monad m, Integral i)
        => Field     -- ^ The name of the field to extract
        -> Form m i
integral field = withForm go
  where
    go (Just (Number scientific)) = convert scientific
    go _ = wrongType
    convert scientific =
        case (floatingOrInteger scientific :: Integral i => Either Double i) of
            Left _    -> wrongType
            Right num -> success num
    wrongType = failed $ errors field ["Must be an integer"]


------------------------------------------------------------------------------
-- | Extracts a field of type 'Double'. If field is not a number validation
-- fails with the message "Must be an number". See also `float` and
-- `realFloat`.
double :: Monad m
       => Field          -- ^ The name of the field to extract
       -> Form m Double
double = realFloat


------------------------------------------------------------------------------
-- | Extracts a field of type 'Double'. If field is not a number validation
-- fails with the message "Must be an number". See also `double` and
-- `realFloat`.
float :: Monad m
      => Field          -- ^ The name of the field to extract
      -> Form m Float
float = realFloat


------------------------------------------------------------------------------
-- | Extracts a field of type 'RealFloat r => r'. If field is not a number
-- validation fails with the message "Must be a number ". See also `float`
-- and `double`.
realFloat :: (Monad m, RealFloat r)
          => Field     -- ^ The name of the field to extract
          -> Form m r
realFloat field = withForm go
  where
    go (Just (Number scientific)) = convert scientific
    go _ = wrongType
    convert scientific =
        case (floatingOrInteger scientific :: RealFloat r => Either r Integer) of
            Left num -> success num
            Right num  -> success $ fromIntegral num
    wrongType = failed $ errors field ["Must be a number"]


------------------------------------------------------------------------------
-- | Extracts a field of type 'Bool'. If field is not a boolean validation
-- fails with the message "Must be a boolean".
bool :: Monad m
       => Field          -- ^ The name of the field to extract
       -> Form m Bool
bool field = withForm go
  where
    go (Just (Bool t)) = success t
    go _ = failed $ errors field ["Must be a boolean"]


------------------------------------------------------------------------------
-- | Extracts a field of type 'Object'. If field is not an object validation
-- fails with the message "Must be an object".
object :: Monad m
       => Field          -- ^ The name of the field to extract
       -> Form m Value
object field = withForm go
  where
    go (Just obj@(Object _)) = success obj
    go _ = failed $ errors field ["Must be an object"]


------------------------------------------------------------------------------
-- | Extracts a field of type 'Object'. If field is not an object validation
-- fails with the message "Must be an array".
array :: Monad m
       => Field          -- ^ The name of the field to extract
       -> Form m Value
array field = withForm go
  where
    go (Just ary@(Array _)) = success ary
    go _ = failed $ errors field ["Must be an array"]


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
