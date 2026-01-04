{-|
Module      : App.Error
Description : Error types and validation for the normalization engine
Copyright   : (c) John Tian, 2026
License     : BSD-3-Clause

Comprehensive error types and input validation for robust error handling.
-}
module App.Error
  ( -- * Error Types
    AppError(..)
  , ValidationError(..)
  , NormalizationError(..)
    -- * Error Constructors
  , validationError
  , normalizationError
  , internalError
    -- * Validation
  , Validated
  , validate
  , validateNonEmpty
  , validateNonEmptyText
  , validateAll
    -- * Error Rendering
  , errorToText
  , errorToStatus
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

-- | Top-level application error
data AppError
  = ValidationErr !ValidationError
  | NormalizationErr !NormalizationError
  | InternalErr !Text
  deriving stock (Eq, Show)

-- | Validation errors for input checking
data ValidationError
  = EmptyField !Text               -- ^ Required field is empty
  | InvalidValue !Text !Text       -- ^ Field name, reason
  | MissingField !Text             -- ^ Required field is missing
  | InvalidFormat !Text !Text      -- ^ Field name, expected format
  | MultipleErrors !(NonEmpty ValidationError)  -- ^ Multiple validation errors
  deriving stock (Eq, Show)

-- | Errors during normalization logic
data NormalizationError
  = InvalidFD !Text                -- ^ Invalid functional dependency
  | EmptyRelation !Text            -- ^ Relation has no attributes
  | CircularDependency !Text       -- ^ Circular FD chain detected
  | DecompositionFailed !Text      -- ^ Algorithm failed
  | InvalidAttribute !Text !Text   -- ^ Unknown attribute
  deriving stock (Eq, Show)

-- | Smart constructors
validationError :: ValidationError -> AppError
validationError = ValidationErr

normalizationError :: NormalizationError -> AppError
normalizationError = NormalizationErr

internalError :: Text -> AppError
internalError = InternalErr

-- | Validation result type
type Validated a = Either ValidationError a

-- | Generic validation combinator
validate :: (a -> Bool) -> ValidationError -> a -> Validated a
validate predicate err value
  | predicate value = Right value
  | otherwise = Left err

-- | Validate a list is non-empty
validateNonEmpty :: Text -> [a] -> Validated [a]
validateNonEmpty fieldName xs
  | null xs = Left $ EmptyField fieldName
  | otherwise = Right xs

-- | Validate text is non-empty
validateNonEmptyText :: Text -> Text -> Validated Text
validateNonEmptyText fieldName txt
  | T.null (T.strip txt) = Left $ EmptyField fieldName
  | otherwise = Right (T.strip txt)

-- | Combine multiple validations, collecting all errors
validateAll :: [Validated a] -> Either ValidationError [a]
validateAll results = 
  case (successes, failures) of
    (_, []) -> Right successes
    (_, [e]) -> Left e
    (_, e:es) -> Left $ MultipleErrors (e :| es)
  where
    successes = [a | Right a <- results]
    failures = [e | Left e <- results]

-- | Convert error to user-friendly text
errorToText :: AppError -> Text
errorToText (ValidationErr ve) = validationErrorToText ve
errorToText (NormalizationErr ne) = normalizationErrorToText ne
errorToText (InternalErr msg) = "Internal error: " <> msg

validationErrorToText :: ValidationError -> Text
validationErrorToText (EmptyField field) = 
  "Field '" <> field <> "' cannot be empty"
validationErrorToText (InvalidValue field reason) = 
  "Invalid value for '" <> field <> "': " <> reason
validationErrorToText (MissingField field) = 
  "Required field '" <> field <> "' is missing"
validationErrorToText (InvalidFormat field expected) = 
  "Invalid format for '" <> field <> "', expected: " <> expected
validationErrorToText (MultipleErrors errs) = 
  T.intercalate "; " (map validationErrorToText (NE.toList errs))

normalizationErrorToText :: NormalizationError -> Text
normalizationErrorToText (InvalidFD reason) = 
  "Invalid functional dependency: " <> reason
normalizationErrorToText (EmptyRelation name) = 
  "Relation '" <> name <> "' has no attributes"
normalizationErrorToText (CircularDependency msg) = 
  "Circular dependency detected: " <> msg
normalizationErrorToText (DecompositionFailed reason) = 
  "Decomposition failed: " <> reason
normalizationErrorToText (InvalidAttribute attr rel) = 
  "Unknown attribute '" <> attr <> "' in relation '" <> rel <> "'"

-- | Convert error to HTTP status code
errorToStatus :: AppError -> Int
errorToStatus (ValidationErr _) = 400   -- Bad Request
errorToStatus (NormalizationErr _) = 422  -- Unprocessable Entity
errorToStatus (InternalErr _) = 500     -- Internal Server Error
