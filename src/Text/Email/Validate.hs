module Text.Email.Validate
    ( canonicalizeEmail
    , emailAddress
    , emailAddressWith
    , isValid
    , isValidWith
    , validate
    , validateWith

    -- Re-exports:
    , EmailAddress
    , EmailAddress'
    , ParseOptions(..)
    , toText
    , unsafeEmailAddress
    ) where

import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Proxy (Proxy(..))
import Data.Text (Text)

import Text.Email.Parser
    ( EmailAddress
    , EmailAddress'
    , ParseOptions(..)
    , defaultParseOptions
    , addrSpec
    , toText
    , unsafeEmailAddress
    )

-- $setup
-- This is required for all examples:
--
-- >>> :set -XOverloadedStrings

-- | Smart constructor for an email address
emailAddress :: Text -> Maybe EmailAddress
emailAddress = emailAddressWith defaultParseOptions

emailAddressWith :: ParseOptions opts => Proxy opts -> Text -> Maybe (EmailAddress' opts)
emailAddressWith opts = either (const Nothing) Just . validateWith opts

-- | Checks that an email is valid and returns a version of it
--   where comments and whitespace have been removed.
--
-- Example:
--
-- >>> canonicalizeEmail "spaces. are. allowed@example.com"
-- Just "spaces.are.allowed@example.com"
canonicalizeEmail :: Text -> Maybe Text
canonicalizeEmail = fmap toText . emailAddress

-- | Validates whether a particular string is an email address
--   according to RFC5322.
isValid :: Text -> Bool
isValid = either (const False) (const True) . validate

-- | Validates whether a particular string is an email address
--   according to RFC5322.
isValidWith :: ParseOptions opts => Proxy opts -> Text -> Bool
isValidWith opts = either (const False) (const True) . validateWith opts

-- | If you want to find out *why* a particular string is not
--   an email address, use this.
--
-- Examples:
--
-- >>> validate "example@example.com"
-- Right "example@example.com"
--
-- >>> validate "not.good"
-- Left "expecting at sign > '@': not enough input"
validate :: Text -> Either String EmailAddress
validate = validateWith defaultParseOptions

validateWith :: ParseOptions opts => Proxy opts -> Text -> Either String (EmailAddress' opts)
validateWith opts = parseOnly (addrSpec opts <* endOfInput)
