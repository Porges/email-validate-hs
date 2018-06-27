{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Email.Parser
    ( EmailAddress
    , EmailAddress'
    , ParseOptions(..)
    , addrSpec
    , AllowAllParseOptions
    , allowAllParseOptions
    , ASCIIOnlyParseOptions
    , asciiOnlyParseOptions
    , DefaultParseOptions
    , defaultParseOptions
    , toAscii
    , toText
    , unsafeEmailAddress
    )
where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad (void, when)
import Data.Attoparsec.Text
import Data.Binary (Binary(..))
import Data.ByteString (ByteString)
import Data.Data (Data, Typeable)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Text.Read as Read

-- $setup
-- Required for all examples:
--
-- >>> :set -XOverloadedStrings

-- | The main email address type. This represents email addresses parsed by the
-- 'DefaultParseOptions' options set.
type EmailAddress = EmailAddress' DefaultParseOptions

-- | Represents an email address, typed by what options it was parsed with. This
-- prevents accidentally mixing up email addresses that were validated under
-- different options.
newtype EmailAddress' opts =
    EmailAddress {
         -- | Get the textual representation of an email address.
        toText :: Text
    } deriving (Eq, Ord, Data, Typeable, Generic)

instance NFData (EmailAddress' opts) where
    rnf (EmailAddress text) = rnf text

instance Binary (EmailAddress' opts) where
    get = EmailAddress <$> get
    put (EmailAddress text) = put text

-- | Creates an email address without validating it.
--   You should only use this when reading data from
--   somewhere it has already been validated (e.g. a
--   database).
--
--   Note that this does no parsing or normalization, so:
--
-- >>> unsafeEmailAddress "me@EXAMPLE.com" == unsafeEmailAddress "me@example.com"
-- False
--
unsafeEmailAddress :: Text -> EmailAddress
unsafeEmailAddress = EmailAddress

-- | Creates an email address without validating it, for any set of validation
-- options.
unsafeEmailAddress' :: Text -> EmailAddress' opts
unsafeEmailAddress' = EmailAddress

instance Show (EmailAddress' opts) where
    show = show . toText

instance ParseOptions opts => Read (EmailAddress' opts) where
    readListPrec = Read.readListPrecDefault
    readPrec = Read.parens (do
        text <- Read.readPrec
        case parseOnly (addrSpec Proxy <* endOfInput) text of
            Left  _ -> Read.pfail
            Right a -> return a)

-- | Converts an email address back to an ASCII-encoded 'ByteString'.
--
-- This will properly encode Unicode domains. Emails with Unicode local-parts
-- cannot be represented in ASCII and will return `Nothing`.
--
-- Example:
--
-- >>> error "TODO: write example"
toAscii :: EmailAddress -> Maybe ByteString
toAscii (EmailAddress _) = error "TODO: Implement toAscii"

class ParseOptions po where
    allowUnicode :: Proxy po -> Bool
    allowObsolete :: Proxy po -> Bool
    normalize :: Proxy po -> Bool
    allowIPHost :: Proxy po -> Bool
    allowHostName :: Proxy po -> Bool

data DefaultParseOptions

defaultParseOptions :: Proxy DefaultParseOptions
defaultParseOptions = Proxy

instance ParseOptions DefaultParseOptions where
    allowUnicode _ = True
    allowObsolete _ = False
    normalize _ = True
    allowIPHost _ = False
    allowHostName _ = False

data ASCIIOnlyParseOptions

asciiOnlyParseOptions :: Proxy ASCIIOnlyParseOptions
asciiOnlyParseOptions = Proxy

instance ParseOptions ASCIIOnlyParseOptions where
    allowUnicode _ = False
    allowObsolete _ = False
    normalize _ = True
    allowIPHost _ = False
    allowHostName _ = False

data AllowAllParseOptions

allowAllParseOptions :: Proxy AllowAllParseOptions
allowAllParseOptions = Proxy

instance ParseOptions AllowAllParseOptions where
    allowUnicode _ = True
    allowObsolete _ = True
    normalize _ = True
    allowIPHost _ = True
    allowHostName _ = True

{-# SPECIALIZE addrSpec :: Proxy DefaultParseOptions -> Parser EmailAddress #-}
{-# SPECIALIZE addrSpec :: Proxy ASCIIOnlyParseOptions -> Parser (EmailAddress' ASCIIOnlyParseOptions) #-}
{-# SPECIALIZE addrSpec :: Proxy AllowAllParseOptions -> Parser (EmailAddress' AllowAllParseOptions) #-}
addrSpec :: (ParseOptions opts) => Proxy opts -> Parser (EmailAddress' opts)
addrSpec opts = do
    l <- local opts <?> "local-part"
    _ <- char '@' <?> "expecting at sign"
    d <- domain opts <?> "domain-part"

    let email = l <> "@" <> d

    -- Maximum length is 254, per Erratum 1690 on RFC3696:
    when (T.length email > 254)
        (fail "Email address is too long (may not exceed 254 octets)")

    return (unsafeEmailAddress' email)

-- | Parser for an email-address local part.
--
-- > The local-part portion is a domain-dependent string.  In addresses, 
-- > it is simply interpreted on the particular host as a name of a
-- > particular mailbox.
--
local :: ParseOptions opts => Proxy opts -> Parser Text
local opts = do
    result <-
        if allowObsolete opts
        then obsLocalPart 
        else dotAtom opts <|> quotedString opts

    -- Maximum length of local-part is 64, per RFC3696
    when (T.length result > 64)
        (fail "Local-part is too long (may not exceed 64 octets)")

    return result

    where 
        obsLocalPart = T.intercalate (T.singleton '.') <$> word `sepBy1` char '.'
        word = atom opts <|> quotedString opts

isUnicodeNonAscii :: ParseOptions opts => Proxy opts -> Char -> Bool
isUnicodeNonAscii opts c =
    allowUnicode opts &&
    -- from RFC3629 section-4, UTF8-{2,3,4}
    (c >= '\x80')
    -- TODO: confirm this

domain :: ParseOptions opts => Proxy opts -> Parser Text
domain opts = domainName opts <|> domainLiteral opts

domainName :: ParseOptions opts => Proxy opts -> Parser Text
domainName opts = do

    parsedDomain <- 
        T.intercalate (T.singleton '.')
        <$> (domainLabel opts) `sepBy1` char '.'
        <* optional (char '.') -- permit trailing dot

    -- Per RFC1035:
    -- Domain name must be no greater than 253 octets

    when (T.length parsedDomain > 253)
        (fail "Domain name is too long (may not exceed 253 octets)")
        -- TODO: this needs to be performed on punycode version!

    return parsedDomain

domainLabel :: ParseOptions opts => Proxy opts -> Parser Text
domainLabel opts = do
    content <- between1 (optional (cfws opts)) (takeWhile1 isAlphaNumHyphen)

    -- Per RFC1035:
    -- A domain label must be no greater than 63 chars and cannot start or end
    -- with a hyphen.

    when (T.head content == '-')
        (fail "Domain label may not start with hyphen")

    when (T.last content == '-')
        (fail "Domain label may not end with hyphen")


    when (T.length content > 63)
        (fail "Domain label is too long (may not exceed 63 octets)")
        -- TODO: this needs to be performed on punycode version!

    return content

{-
alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum
-}

isAlphaNumHyphen :: Char -> Bool
isAlphaNumHyphen c = isAlphaNum c || c == '-'

{- | == Atom (3.2.3) -}

isAtomText :: ParseOptions opts => Proxy opts -> Char -> Bool
isAtomText opts c =
    isAlphaNum c ||
    inClass "!#$%&'*+/=?^_`{|}~-" c ||
    isUnicodeNonAscii opts c -- RFC6532 addition

atom :: ParseOptions opts => Proxy opts -> Parser Text
atom opts = between1 (cfws opts) (takeWhile1 (isAtomText opts))

dotAtomText :: ParseOptions opts => Proxy opts -> Parser Text
dotAtomText opts =
    (T.intercalate (T.singleton '.')) <$>
    (takeWhile1 (isAtomText opts)) `sepBy1` (char '.')

dotAtom :: ParseOptions opts => Proxy opts -> Parser Text
dotAtom opts = between1 (cfws opts) (dotAtomText opts)

{-
dottedAtoms :: Parser ByteString
dottedAtoms = T.intercalate (T.singleton '.') <$>
        between1 (optional cfws) atom `sepBy1` char '.'
    -}

domainLiteral :: ParseOptions opts => Proxy opts -> Parser Text
domainLiteral opts = 
    (T.cons '[' . flip T.snoc ']') <$>
    between (optional (cfws opts) *> char '[') (char ']' <* optional (cfws opts))
        parseIP

    where
    parseIP =
        if allowIPHost opts
        then (("IPv6:" <>) . IPv6.encode) <$> (string "IPv6:" *> IPv6.parser) <|> IPv4.encode <$> IPv4.parser
        else fail "IP host not permitted"

isDomainText :: ParseOptions opts => Proxy opts -> Char -> Bool
isDomainText opts c =
    inClass "\33-\90\94-\126" c ||
    isObsNoWsCtl opts c

{- | == Quoted Strings (3.2.4) -}

isQuotedText :: ParseOptions opts => Proxy opts -> Char -> Bool
isQuotedText opts c =
    (c >= '\35' && c <= '\91') ||
    (c >= '\93' && c <= '\126') ||
    (c == '\33') ||
    isUnicodeNonAscii opts c || -- RFC6532 addition
    isObsNoWsCtl opts c

quotedContent :: ParseOptions opts => Proxy opts -> Parser Text
quotedContent opts = takeWhile1 (isQuotedText opts) <|> quotedPair opts

quotedString :: ParseOptions opts => Proxy opts -> Parser Text
quotedString opts = do
    between1 (cfws opts) $
        between1 (char '"') $ do
            quotedParts <- many (mappend <$> option mempty fws' <*> quotedContent opts)
            endWS <- option mempty fws'

            -- TODO: check if it must be quoted and either keep or remove
            -- quotes, subject to normalization
            return (mconcat ("\"" : quotedParts ++ [endWS, "\""]))

{- | == Quoted characters (3.2.1) -}

quotedPair :: ParseOptions opts => Proxy opts -> Parser Text
quotedPair opts =
    (T.cons '\\' . T.singleton) <$>
    (char '\\' *> qp)

    where
    qp = vchar <|> wsp <|> obsQp

    obsQp = 
        if allowObsolete opts
        then lf <|> cr <|> satisfy (isObsNoWsCtl opts) <|> nullChar
        else empty

{- | == Folding White Space and Comments (3.2.2) -}

-- | Folding whitespace, where it should be ignored.
fws :: Parser ()
fws = void (wsp1 >> optional (crlf >> wsp1)) <|> (skipMany1 (crlf >> wsp1))

-- | Folding whitespace, where it is significant (i.e. inside a quoted-string)
fws' :: Parser Text
fws' = do
    (ws, ()) <- match fws
    return (T.pack (stripCRLF (T.unpack ws)))

    where 
    stripCRLF ('\r' : '\n' : xs) = stripCRLF xs
    stripCRLF (x : xs) = x : stripCRLF xs
    stripCRLF [] = []

isCommentText :: ParseOptions opts => Proxy opts -> Char -> Bool
isCommentText opts c =
    inClass "\33-\39\42-\91\93-\126" c ||
    isUnicodeNonAscii opts c || -- RFC6532 addition
    isObsNoWsCtl opts c

commentContent :: ParseOptions opts => Proxy opts -> Parser ()
commentContent opts =
    skipWhile1 (isCommentText opts) <|>
    void (quotedPair opts) <|>
    comment opts

comment :: ParseOptions opts => Proxy opts -> Parser ()
comment opts =
    between (char '(') (char ')') $
        skipMany (void (commentContent opts) <|> fws)

cfws :: ParseOptions opts => Proxy opts -> Parser ()
cfws opts = skipMany (comment opts <|> fws)

{- Helpers -}

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
isAlphaNum c =
    (c >= 'a' && c <= 'z') ||
    (c >= '0' && c <= '9') ||
    (c >= 'A' && c <= 'Z')

cr :: Parser Char
cr = char '\r'

lf :: Parser Char
lf = char '\n'

crlf :: Parser ()
crlf = void (cr *> lf)

isVchar :: Char -> Bool
isVchar = inClass "\x21-\x7e"

vchar :: Parser Char
vchar = satisfy isVchar

isObsNoWsCtl :: ParseOptions opts => Proxy opts -> Char -> Bool
isObsNoWsCtl opts c =
    allowObsolete opts && (inClass "\1-\8\11\12\14-\31\127" c)

between :: Applicative f => f l -> f r -> f a -> f a
between l r x = l *> x <* r

between1 :: Applicative f => f lr -> f a -> f a
between1 lr x = lr *> x <* lr

