module Text.Email.Validate
        ( isValid
        , validate
        , emailAddress
        , canonicalizeEmail
        , EmailAddress -- re-exported
        , unsafeEmailAddress
        , localPart
        , domainPart
        , toByteString
        )
where

import           Data.Attoparsec.ByteString (endOfInput, parseOnly)
import           Data.ByteString            (ByteString)

import           Text.Email.Parser          (EmailAddress, addrSpec, domainPart,
                                             localPart, toByteString, unsafeEmailAddress)

-- | Smart constructor for an email address
emailAddress :: ByteString -> Maybe EmailAddress
emailAddress = either (const Nothing) Just . validate

-- | Checks that an email is valid and returns a version of it
--   where comments and whitespace have been removed.
canonicalizeEmail :: ByteString -> Maybe ByteString
canonicalizeEmail = fmap toByteString . emailAddress

-- | Validates whether a particular string is an email address
--   according to RFC5322.
isValid :: ByteString -> Bool
isValid = either (const False) (const True) . validate

-- | If you want to find out *why* a particular string is not
--   an email address, use this.
validate :: ByteString -> Either String EmailAddress
validate = parseOnly (addrSpec <* endOfInput)
