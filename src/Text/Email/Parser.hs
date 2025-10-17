{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Text.Email.Parser
    ( addrSpec
    , localPart
    , domainPart
    , EmailAddress
    , unsafeEmailAddress
    , toByteString
    )
where

import           Control.Applicative
import           Control.Monad (guard, void, when)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (ord)
import           Data.Data (Data, Typeable)
import           GHC.Generics (Generic)
import qualified Text.Read as Read

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

-- | A parser for email addresses.
addrSpec :: Parser EmailAddress
addrSpec = do
    l <- local

    -- Maximum length of local-part is 64, per RFC3696
    when (BS.length l > 64) (fail "local-part of email is too long (more than 64 octets)")

    _ <- char '@' <?> "at sign"
    d <- domain

    -- Maximum length is 254, per Erratum 1690 on RFC3696
    when (BS.length l + BS.length d + 1 > 254) (fail "email address is too long (more than 254 octets)")

    return (unsafeEmailAddress l d)

local :: Parser ByteString
local = dottedAtoms

domain :: Parser ByteString
domain = domainName <|> domainLiteral

domainName :: Parser ByteString
domainName = do
    parsedDomain <- BS.intercalate (BS.singleton '.') <$>
        domainLabel `sepBy1` char '.' <* optional (char '.')

    -- Domain name must be no greater than 253 chars, per RFC1035
    guard (BS.length parsedDomain <= 253)
    return parsedDomain

domainLabel :: Parser ByteString
domainLabel = do
    content <- between1 (optional cfws) (fst <$> match (alphaNum >> skipWhile isAlphaNumHyphen))

    -- Per RFC1035:
    -- label must be no greater than 63 chars and cannot end with '-'
    -- (we already enforced that it does not start with '-')
    guard (BS.length content <= 63 && BS.last content /= '-')
    return content

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

isAlphaNumHyphen :: Char -> Bool
isAlphaNumHyphen x = isDigit x || isAlpha_ascii x || x == '-'

dottedAtoms :: Parser ByteString
dottedAtoms = BS.intercalate (BS.singleton '.') <$>
        between1 (optional cfws)
            (atom <|> quotedString) `sepBy1` char '.'

atom :: Parser ByteString
atom = BS.concat <$> many1 (takeWhile1 isAtomText <|> nonAsciiUtf8)

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
    (BS.cons '"' . flip BS.snoc '"' . BS.concat) <$>
        between1 (char '"')
            (many (optional fws >> quotedContent) <* optional fws)

quotedContent :: Parser ByteString
quotedContent = takeWhile1 isQuotedText <|> quotedPair <|> nonAsciiUtf8

isQuotedText :: Char -> Bool
isQuotedText x = inClass "\33\35-\91\93-\126" x || isObsNoWsCtl x

quotedPair :: Parser ByteString
quotedPair = char '\\' *> (BS.cons '\\' <$>
  ((BS.singleton <$> (vchar <|> wsp <|> lf <|> cr <|> obsNoWsCtl <|> nullChar))
  <|> nonAsciiUtf8))

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
commentContent = skipWhile1 isCommentText <|> void quotedPair <|> void nonAsciiUtf8 <|> comment

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

nonAsciiUtf8 :: Parser ByteString
nonAsciiUtf8 = utf8Pair <|> utf8Triple <|> utf8Quad

utf8Pair :: Parser ByteString
utf8Pair = convert <$> satisfy byteOne <*> satisfy byteTwo
  where
    convert :: Char -> Char -> ByteString
    convert high low = BS.pack [high, low]
    byteOne x =
      let ox = ord x
       in ox >= 0xc0 && ox <= 0xdf
    byteTwo x =
      let ox = ord x
       in ox >= 0x80 && ox <= 0xbf

utf8Triple :: Parser ByteString
utf8Triple = convert <$> satisfy byteOne <*> satisfy byteLater <*> satisfy byteLater
  where
    convert :: Char -> Char -> Char -> ByteString
    convert high middle low = BS.pack [high, middle, low]
    byteOne x =
      let ox = ord x
       in ox >= 0xe0 && ox <= 0xef
    byteLater x =
      let ox = ord x
       in ox >= 0x80 && ox <= 0xbf

utf8Quad :: Parser ByteString
utf8Quad = convert <$> satisfy byteOne <*> satisfy byteLater <*> satisfy byteLater <*> satisfy byteLater
  where
    convert :: Char -> Char -> Char -> Char -> ByteString
    convert high middleHigh middleLow low = BS.pack [high, middleHigh, middleLow, low]
    byteOne x =
      let ox = ord x
       in ox >= 0xf0 && ox <= 0xf7
    byteLater x =
      let ox = ord x
       in ox >= 0x80 && ox <= 0xbf

isObsNoWsCtl :: Char -> Bool
isObsNoWsCtl = inClass "\1-\8\11-\12\14-\31\127"

obsNoWsCtl :: Parser Char
obsNoWsCtl = satisfy isObsNoWsCtl
