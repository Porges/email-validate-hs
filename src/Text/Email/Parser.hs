module Text.Email.Parser
	(addrSpec
	,localPart
	,domainPart
	,EmailAddress
	,toByteString)
where

import Control.Applicative

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

import Data.Char (chr)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator

import qualified Text.Read as Read

-- | Represents an email address.
data EmailAddress = EmailAddress ByteString ByteString deriving (Eq,Ord)

instance Show EmailAddress where
	show = show . toByteString

instance Read EmailAddress where
	readListPrec = Read.readListPrecDefault
	readPrec = Read.parens (do
		bs <- Read.readPrec
		case parseOnly addrSpec bs of
			Left  _ -> Read.pfail
			Right a -> return a)

-- | Converts an email address back to a ByteString
toByteString (EmailAddress l d) = BS.concat [l, BS.singleton '@', d]

-- | Extracts the local part of an email address.
localPart :: EmailAddress -> ByteString
localPart (EmailAddress local _) = local

-- | Extracts the domain part of an email address.
domainPart :: EmailAddress -> ByteString
domainPart (EmailAddress _ domain) = domain

-- | A parser for email addresses.
addrSpec = do
	localPart <- local
	char '@'
	domainPart <- domain
	endOfInput
	return (EmailAddress localPart domainPart)

local = dottedAtoms
domain = dottedAtoms <|> domainLiteral

dottedAtoms = BS.intercalate (BS.singleton '.') <$>
	(optional cfws *> (atom <|> quotedString) <* optional cfws)	`sepBy1` (char '.')
atom = takeWhile1 isAtomText

isAtomText x = isAlphaNum x || inClass "!#$%&'*+/=?^_`{|}~-" x

domainLiteral = (BS.cons '[' . flip BS.snoc ']' . BS.concat) <$> (between (optional cfws *> char '[') (char ']' <* optional cfws) $
	many (optional fws >> takeWhile1 isDomainText) <* optional fws)
isDomainText x = inClass "\33-\90\94-\126" x || isObsNoWsCtl x

quotedString = (\x -> BS.concat $ [BS.singleton '"', BS.concat x, BS.singleton '"']) <$> (between (char '"') (char '"') $
	many (optional fws >> quotedContent) <* optional fws)

quotedContent = takeWhile1 isQuotedText <|> quotedPair
isQuotedText x = inClass "\33\35-\91\93-\126" x || isObsNoWsCtl x

quotedPair = (BS.cons '\\' . BS.singleton) <$> (char '\\' *> (vchar <|> wsp <|> lf <|> cr <|> obsNoWsCtl <|> nullChar))

cfws = ignore $ many (comment <|> fws)

fws :: Parser ()
fws = ignore $
	ignore (wsp1 >> optional (crlf >> wsp1))
	<|> ignore (many1 (crlf >> wsp1))

ignore :: Parser a -> Parser ()
ignore x = x >> return ()

between l r x = l *> x <* r

comment :: Parser ()
comment = ignore ((between (char '(') (char ')') $
	many (ignore commentContent <|> fws)))

commentContent = skipWhile1 isCommentText <|> ignore quotedPair <|> comment
isCommentText x = inClass "\33-\39\42-\91\93-\126" x || isObsNoWsCtl x

nullChar = char '\0'

skipWhile1 x = satisfy x >> skipWhile x

wsp1 = skipWhile1 isWsp
wsp = satisfy isWsp
isWsp x = x == ' ' || x == '\t'

isAlphaNum x = isDigit x || isAlpha_ascii x
cr = char '\r'
lf = char '\n'
crlf = cr >> lf >> return ()

isVchar = inClass "\x21-\x7e"
vchar = satisfy isVchar

isObsNoWsCtl = inClass "\1-\8\11-\12\14-\31\127"
obsNoWsCtl = satisfy isObsNoWsCtl