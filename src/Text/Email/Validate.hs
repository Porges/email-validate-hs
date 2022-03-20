module Text.Email.Validate
    ( isValid
    , validate
    , emailAddress
    , canonicalizeEmail

    -- Re-exports:
    , EmailAddress
    , domainPart
    , localPart
    , toByteString
    , unsafeEmailAddress
    )
where

import Data.Attoparsec.ByteString (endOfInput, parseOnly)
import Data.ByteString (ByteString)

import Text.Email.Parser
    ( EmailAddress
    , addrSpec
    , domainPart
    , localPart
    , toByteString
    , unsafeEmailAddress)

-- | Smart constructor for an email address
emailAddress :: ByteString -> Maybe EmailAddress
emailAddress = either (const Nothing) Just . validate

-- | Checks that an email is valid and returns a version of it
--   where comments and whitespace have been removed.
--
-- Example (requires `OverloadedStrings` to be enabled):
--
-- >>> canonicalizeEmail "spaces. are. allowed@example.com"
-- Just "spaces.are.allowed@example.com"
canonicalizeEmail :: ByteString -> Maybe ByteString
canonicalizeEmail = fmap toByteString . emailAddress

-- | Validates whether a particular string is an email address
--   according to RFC5322.
isValid :: ByteString -> Bool
isValid = either (const False) (const True) . validate

-- | If you want to find out *why* a particular string is not
--   an email address, use this.
--
-- Examples (both require `OverloadedStrings` to be enabled):
--
-- >>> validate "example@example.com"
-- Right "example@example.com"
--
-- >>> validate "not.good"
-- Left "at sign > @: not enough input"
validate :: ByteString -> Either String EmailAddress
validate = parseOnly (addrSpec >>= \r -> endOfInput >> return r)
