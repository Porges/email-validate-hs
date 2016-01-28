{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.EmailAddress
    ( localPart
    , domainPart
    , EmailAddress
    , unsafeEmailAddress
    , toByteString
    )
where

import           Control.Applicative ((<*))
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Data (Data, Typeable)
import           GHC.Generics (Generic)
import qualified Text.Read as Read

import {-# SOURCE #-} Text.Email.Parser (addrSpec)

-- | Represents an email address.
data EmailAddress = EmailAddress ByteString ByteString
    deriving (Eq, Ord, Data, Typeable, Generic)

-- | Creates an email address without validating it.
--   You should only use this when reading data from
--   somewhere it has already been validated (e.g. a
--   database).
unsafeEmailAddress :: ByteString -> ByteString -> EmailAddress
unsafeEmailAddress = EmailAddress

instance Show EmailAddress where
    show = show . toByteString

instance Read EmailAddress where
    readListPrec = Read.readListPrecDefault
    readPrec = Read.parens (do
        bs <- Read.readPrec
        case parseOnly (addrSpec <* endOfInput) bs of
            Left  _ -> Read.pfail
            Right a -> return a)

-- | Converts an email address back to a ByteString
toByteString :: EmailAddress -> ByteString
toByteString (EmailAddress l d) = BS.concat [l, BS.singleton '@', d]

-- | Extracts the local part of an email address.
localPart :: EmailAddress -> ByteString
localPart (EmailAddress l _) = l

-- | Extracts the domain part of an email address.
domainPart :: EmailAddress -> ByteString
domainPart (EmailAddress _ d) = d