{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Text.Email.Parser
    ( addrSpec
    , localPart
    , domainPart
    , EmailAddress
    , toByteString
    )
where

import Control.Applicative
import Control.Monad (void)

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

import Data.Attoparsec.ByteString.Char8

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import qualified Text.Read as Read

-- | Represents an email address.
data EmailAddress = EmailAddress ByteString ByteString
    deriving (Eq, Ord, Data, Typeable, Generic)

instance Show EmailAddress where
    show = show . toByteString

instance Read EmailAddress where
    readsPrec _ s = go (parse addrSpec $ BS.pack s) where
        go (Fail _ _ _) = []
        go (Partial f)  = go (f BS.empty)
        go (Done r adr) = [(adr, BS.unpack r)]

-- | Converts an email address back to a ByteString
toByteString :: EmailAddress -> ByteString
toByteString (EmailAddress l d) = BS.concat [l, BS.singleton '@', d]

-- | Extracts the local part of an email address.
localPart :: EmailAddress -> ByteString
localPart (EmailAddress l _) = l

-- | Extracts the domain part of an email address.
domainPart :: EmailAddress -> ByteString
domainPart (EmailAddress _ d) = d

-- | A parser for email addresses.
addrSpec :: Parser EmailAddress
addrSpec = do
    l <- local
    _ <- char '@'
    d <- domain
    return (EmailAddress l d)

local :: Parser ByteString
local = dottedAtoms

domain :: Parser ByteString
domain = dottedAtoms <|> domainLiteral

dottedAtoms :: Parser ByteString
dottedAtoms = BS.intercalate (BS.singleton '.') <$>
        between1 (optional cfws) (atom <|> quotedString) `sepBy1` char '.'

atom :: Parser ByteString
atom = takeWhile1 isAtomText

isAtomText :: Char -> Bool
isAtomText x = isAlphaNum x || inClass "!#$%&'*+/=?^_`{|}~-" x

domainLiteral :: Parser ByteString
domainLiteral =
    (BS.cons '[' . flip BS.snoc ']' . BS.concat) <$>
        between (optional cfws *> char '[') (char ']' <* optional cfws)
            (many (optional fws >> takeWhile1 isDomainText) <* optional fws)

isDomainText :: Char -> Bool
isDomainText x = inClass "\33-\90\94-\126" x || isObsNoWsCtl x

quotedString :: Parser ByteString
quotedString =
    (\x -> BS.concat [BS.singleton '"', BS.concat x, BS.singleton '"']) <$>
        between (char '"') (char '"')
            (many (optional fws >> quotedContent) <* optional fws)

quotedContent :: Parser ByteString
quotedContent = takeWhile1 isQuotedText <|> quotedPair

isQuotedText :: Char -> Bool
isQuotedText x = inClass "\33\35-\91\93-\126" x || isObsNoWsCtl x

quotedPair :: Parser ByteString
quotedPair = (BS.cons '\\' . BS.singleton) <$> (char '\\' *> (vchar <|> wsp <|> lf <|> cr <|> obsNoWsCtl <|> nullChar))

cfws :: Parser ()
cfws = skipMany (comment <|> fws)

fws :: Parser ()
fws = void (wsp1 >> optional (crlf >> wsp1)) <|> (skipMany1 (crlf >> wsp1))

between :: Applicative f => f l -> f r -> f a -> f a
between l r x = l *> x <* r

between1 :: Applicative f => f lr -> f a -> f a
between1 lr x = lr *> x <* lr

comment :: Parser ()
comment = between (char '(') (char ')') $ skipMany (void commentContent <|> fws)

commentContent :: Parser ()
commentContent = skipWhile1 isCommentText <|> void quotedPair <|> comment

isCommentText :: Char -> Bool
isCommentText x = inClass "\33-\39\42-\91\93-\126" x || isObsNoWsCtl x

nullChar :: Parser Char
nullChar = char '\0'

skipWhile1 :: (Char -> Bool) -> Parser()
skipWhile1 x = satisfy x >> skipWhile x

wsp1 :: Parser ()
wsp1 = skipWhile1 isWsp

wsp :: Parser Char
wsp = satisfy isWsp

isWsp :: Char -> Bool
isWsp x = x == ' ' || x == '\t'

isAlphaNum :: Char -> Bool
isAlphaNum x = isDigit x || isAlpha_ascii x

cr :: Parser Char
cr = char '\r'

lf :: Parser Char
lf = char '\n'

crlf :: Parser ()
crlf = void $ cr >> lf

isVchar :: Char -> Bool
isVchar = inClass "\x21-\x7e"

vchar :: Parser Char
vchar = satisfy isVchar

isObsNoWsCtl :: Char -> Bool
isObsNoWsCtl = inClass "\1-\8\11-\12\14-\31\127"

obsNoWsCtl :: Parser Char
obsNoWsCtl = satisfy isObsNoWsCtl
