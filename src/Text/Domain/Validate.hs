module Text.Domain.Validate
        ( isValid
        , validate
        , domainName
        , DomainName
        , toByteString
        )
where

import           Control.Applicative        ((<*))

import           Data.Attoparsec.ByteString (endOfInput, parseOnly)
import           Data.ByteString            (ByteString)

import           Data.Data                  (Data, Typeable)
import           GHC.Generics               (Generic)
import           Text.Email.Parser          (dottedAtoms)
import qualified Text.Read                  as Read


-- | Represents a domain name.
data DomainName = DomainName ByteString
    deriving (Eq, Ord, Data, Typeable, Generic)

-- | Smart constructor for a domain name
domainName :: ByteString -> Maybe DomainName
domainName = either (const Nothing) Just . validate

-- | Validates whether a particular string is a domain name
--   according to RFC5322.
isValid :: ByteString -> Bool
isValid = either (const False) (const True) . validate

-- | If you want to find out *why* a particular string is not
--   a domain name, use this.

validate :: ByteString -> Either String DomainName
validate = fmap DomainName . parseOnly (dottedAtoms <* endOfInput)

instance Show DomainName where
    show = show . toByteString

instance Read DomainName where
    readListPrec = Read.readListPrecDefault
    readPrec = Read.parens (do
        bs <- Read.readPrec
        case parseOnly (dottedAtoms <* endOfInput) bs of
            Left  _ -> Read.pfail
            Right a -> return $ DomainName a)

-- | Converts an email address back to a ByteString
toByteString :: DomainName -> ByteString
toByteString (DomainName d) = d
