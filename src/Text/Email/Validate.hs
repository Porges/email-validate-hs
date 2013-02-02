module Text.Email.Validate
	(isValid
	,validate
	,emailAddress
	,canonicalizeEmail 
	,EmailAddress -- re-exported
	,localPart
	,domainPart
	,toByteString) 
where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Attoparsec (parseOnly)

import Text.Email.Parser

-- | Smart constructor for an email address
emailAddress :: ByteString -> Maybe EmailAddress
emailAddress x =
	case validate x of
		Left _  -> Nothing
		Right em -> Just em

-- | Checks that an email is valid and returns a version of it
--   where comments and whitespace have been removed.
canonicalizeEmail :: ByteString -> Maybe ByteString
canonicalizeEmail = fmap toByteString . emailAddress

-- | Validates whether a particular string is an email address
--   according to RFC5322.
isValid :: ByteString -> Bool
isValid x = let result = validate x in
	either (const False) (const True) result

-- | If you want to find out *why* a particular string is not
--   an email address, use this.
validate :: ByteString -> Either String EmailAddress
validate = parseOnly addrSpec
