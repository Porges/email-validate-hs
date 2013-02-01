module Text.Email.Validate (addrSpec,isValid,validate,EmailAddress,domainPart,localPart,toByteString,canonicalizeEmail)
where

import Control.Applicative
import Control.Arrow ((***))
import Data.Char (chr)

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator

data EmailAddress = EmailAddress ByteString ByteString deriving (Eq,Ord)

instance Show EmailAddress where
	show = BS.unpack . toByteString

toByteString (EmailAddress l d) = BS.concat [l, BS.pack "@", d]

-- | Smart constructor for an email address
emailAddress :: ByteString -> Maybe EmailAddress
emailAddress x =
	case validate x of
		Left _  -> Nothing
		Right em -> Just em

canonicalizeEmail :: ByteString -> Maybe ByteString
canonicalizeEmail = fmap toByteString . emailAddress

localPart :: EmailAddress -> ByteString
localPart (EmailAddress local _) = local

domainPart :: EmailAddress -> ByteString
domainPart (EmailAddress _ domain) = domain

-- | Validates whether a particular string is an email address
--   according to RFC5322.
isValid :: ByteString -> Bool
isValid x = let result = validate x in
	either (const False) (const True) result

-- | If you want to find out why a particular string is not
--   an email address, use this!
validate :: ByteString -> Either String EmailAddress
validate = parseOnly addrSpec

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
atomText = satisfy isAtomText

domainLiteral = (BS.cons '[' . flip BS.snoc ']' . BS.pack) <$> (between (optional cfws *> char '[') (char ']' <* optional cfws) $
	many (optional fws >> domainText) <* optional fws)
domainText = charInClass "\33-\90\94-\126" <|> obsNoWsCtl

charInClass c = satisfy (inClass c) <?> "one of the following: " ++ c

quotedString = (\x -> BS.concat $ [BS.pack "\"", BS.concat x, BS.pack "\""]) <$> (between (char '"') (char '"') $
	many (optional fws >> quotedContent) <* optional fws)
quotedContent = (BS.singleton <$> quotedText) <|> quotedPair
quotedText = charInClass "\33\35-\91\93-\126" <|> obsNoWsCtl
quotedPair = (BS.cons '\\' . BS.singleton) <$> (char '\\' *> (vchar <|> wsp <|> lf <|> cr <|> obsNoWsCtl <|> nullChar))

fws :: Parser ()
fws = ignore $
	ignore (wsp1 >> optional (crlf >> wsp1))
	<|> ignore (many1 (crlf >> wsp1))

ignore :: Parser a -> Parser ()
ignore x = x >> return ()

between l r x = l *> x <* r

cfws = many (ignore comment <|> fws) >> return BS.empty

comment :: Parser ()
comment = ignore ((between (char '(') (char ')') $
	many (ignore commentContent <|> fws)))

commentContent = (BS.singleton <$> commentText) <|> quotedPair <|> (comment >> return BS.empty)
commentText = charInClass "\33-\39\42-\91\93-\126" <|> obsNoWsCtl

nullChar = char '\0'

skipWhile1 x = satisfy x >> skipWhile x

wsp1 = skipWhile1 isWsp
wsp = satisfy isWsp

isWsp x = x == ' ' || x == '\t'

isAlphaNum x = isDigit x || isAlpha_ascii x
cr = char '\r'
lf = char '\n'
crlf = (const $ BS.pack "\r\n") <$> cr *> lf
vchar = charInClass "\x21-\x7e"
obsNoWsCtl = charInClass "\1-\8\11-\12\14-\31\127"

unitTest (x, y, z) = if not (isValid (BS.pack x) == y) then "" else (x ++" became "++ (case emailAddress (BS.pack x) of {Nothing -> "fail"; Just em -> show em}) ++": Should be "++show y ++", got "++show (not y)++"\n\t"++z++"\n")

doSomeTests = do
	putStr$unitTest("first.last@example.com", True, "")
	putStr$unitTest("1234567890123456789012345678901234567890123456789012345678901234@example.com", True, "")
	putStr$unitTest("\"first last\"@example.com", True, "")
	putStr$unitTest("\"first\\\"last\"@example.com", True, "")
	putStr$unitTest("first\\@last@example.com", False, "Escaping can only happen within a quoted string")
	putStr$unitTest("\"first@last\"@example.com", True, "")
	putStr$unitTest("\"first\\\\last\"@example.com", True, "")
	putStr$unitTest("x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x234", True, "")
	putStr$unitTest("123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.123456789012345678901234567890123456789012345678901234567890123.example.com", True, "")
	putStr$unitTest("first.last@[12.34.56.78]", True, "")
	putStr$unitTest("first.last@[IPv6:::12.34.56.78]", True, "")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333::4444:12.34.56.78]", True, "")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.56.78]", True, "")
	putStr$unitTest("first.last@[IPv6:::1111:2222:3333:4444:5555:6666]", True, "")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333::4444:5555:6666]", True, "")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333:4444:5555:6666::]", True, "")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]", True, "")
	putStr$unitTest("first.last@x23456789012345678901234567890123456789012345678901234567890123.example.com", True, "")
	putStr$unitTest("first.last@1xample.com", True, "")
	putStr$unitTest("first.last@123.example.com", True, "")
	putStr$unitTest("123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.1234.example.com", False, "Entire address is longer than 256 characters")
	putStr$unitTest("first.last", False, "No @")
	putStr$unitTest("12345678901234567890123456789012345678901234567890123456789012345@example.com", False, "Local part more than 64 characters")
	putStr$unitTest(".first.last@example.com", False, "Local part starts with a dot")
	putStr$unitTest("first.last.@example.com", False, "Local part ends with a dot")
	putStr$unitTest("first..last@example.com", False, "Local part has consecutive dots")
	putStr$unitTest("\"first\"last\"@example.com", False, "Local part contains unescaped excluded characters")
	putStr$unitTest("\"first\\last\"@example.com", True, "Any character can be escaped in a quoted string")
	putStr$unitTest("\"\"\"@example.com", False, "Local part contains unescaped excluded characters")
	putStr$unitTest("\"\\\"@example.com", False, "Local part cannot end with a backslash")
	putStr$unitTest("\"\"@example.com", False, "Local part is effectively empty")
	putStr$unitTest("first\\\\@last@example.com", False, "Local part contains unescaped excluded characters")
	putStr$unitTest("first.last@", False, "No domain")
	putStr$unitTest("x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456", False, "Domain exceeds 255 chars")
	putStr$unitTest("first.last@[.12.34.56.78]", False, "Only char that can precede IPv4 address is \':\'")
	putStr$unitTest("first.last@[12.34.56.789]", False, "Can\'t be interpreted as IPv4 so IPv6 tag is missing")
	putStr$unitTest("first.last@[::12.34.56.78]", False, "IPv6 tag is missing")
	putStr$unitTest("first.last@[IPv5:::12.34.56.78]", False, "IPv6 tag is wrong")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333::4444:5555:12.34.56.78]", False, "Too many IPv6 groups (4 max)")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333:4444:5555:12.34.56.78]", False, "Not enough IPv6 groups")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:12.34.56.78]", False, "Too many IPv6 groups (6 max)")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777]", False, "Not enough IPv6 groups")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888:9999]", False, "Too many IPv6 groups (8 max)")
	putStr$unitTest("first.last@[IPv6:1111:2222::3333::4444:5555:6666]", False, "Too many \'::\' (can be none or one)")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333::4444:5555:6666:7777]", False, "Too many IPv6 groups (6 max)")
	putStr$unitTest("first.last@[IPv6:1111:2222:333x::4444:5555]", False, "x is not valid in an IPv6 address")
	putStr$unitTest("first.last@[IPv6:1111:2222:33333::4444:5555]", False, "33333 is not a valid group in an IPv6 address")
	putStr$unitTest("first.last@example.123", False, "TLD can\'t be all digits")
	putStr$unitTest("first.last@com", False, "Mail host must be second- or lower level")
	putStr$unitTest("first.last@-xample.com", False, "Label can\'t begin with a hyphen")
	putStr$unitTest("first.last@exampl-.com", False, "Label can\'t end with a hyphen")
	putStr$unitTest("first.last@x234567890123456789012345678901234567890123456789012345678901234.example.com", False, "Label can\'t be longer than 63 octets")
	putStr$unitTest("\"Abc\\@def\"@example.com", True, "")
	putStr$unitTest("\"Fred\\ Bloggs\"@example.com", True, "")
	putStr$unitTest("\"Joe.\\\\Blow\"@example.com", True, "")
	putStr$unitTest("\"Abc@def\"@example.com", True, "")
	putStr$unitTest("\"Fred Bloggs\"@example.com", True, "")
	putStr$unitTest("user+mailbox@example.com", True, "")
	putStr$unitTest("customer/department=shipping@example.com", True, "")
	putStr$unitTest("$A12345@example.com", True, "")
	putStr$unitTest("!def!xyz%abc@example.com", True, "")
	putStr$unitTest("_somename@example.com", True, "")
	putStr$unitTest("dclo@us.ibm.com", True, "")
	putStr$unitTest("abc\\@def@example.com", False, "This example from RFC3696 was corrected in an erratum")
	putStr$unitTest("abc\\\\@example.com", False, "This example from RFC3696 was corrected in an erratum")
	putStr$unitTest("peter.piper@example.com", True, "")
	putStr$unitTest("Doug\\ \\\"Ace\\\"\\ Lovell@example.com", False, "Escaping can only happen in a quoted string")
	putStr$unitTest("\"Doug \\\"Ace\\\" L.\"@example.com", True, "")
	putStr$unitTest("abc@def@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("abc\\\\@def@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("abc\\@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("@example.com", False, "No local part")
	putStr$unitTest("doug@", False, "Doug Lovell says this should fail")
	putStr$unitTest("\"qu@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("ote\"@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest(".dot@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("dot.@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("two..dot@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("\"Doug \"Ace\" L.\"@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("Doug\\ \\\"Ace\\\"\\ L\\.@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("hello world@example.com", False, "Doug Lovell says this should fail")
	putStr$unitTest("gatsby@f.sc.ot.t.f.i.tzg.era.l.d.", False, "Doug Lovell says this should fail")
	putStr$unitTest("test@example.com", True, "")
	putStr$unitTest("TEST@example.com", True, "")
	putStr$unitTest("1234567890@example.com", True, "")
	putStr$unitTest("test+test@example.com", True, "")
	putStr$unitTest("test-test@example.com", True, "")
	putStr$unitTest("t*est@example.com", True, "")
	putStr$unitTest("+1~1+@example.com", True, "")
	putStr$unitTest("{_test_}@example.com", True, "")
	putStr$unitTest("\"[[ test ]]\"@example.com", True, "")
	putStr$unitTest("test.test@example.com", True, "")
	putStr$unitTest("\"test.test\"@example.com", True, "")
	putStr$unitTest("test.\"test\"@example.com", True, "Obsolete form, but documented in RFC2822")
	putStr$unitTest("\"test@test\"@example.com", True, "")
	putStr$unitTest("test@123.123.123.x123", True, "")
	putStr$unitTest("test@123.123.123.123", False, "Top Level Domain won\'t be all-numeric (see RFC3696 Section 2). I disagree with Dave Child on this one.")
	putStr$unitTest("test@[123.123.123.123]", True, "")
	putStr$unitTest("test@example.example.com", True, "")
	putStr$unitTest("test@example.example.example.com", True, "")
	putStr$unitTest("test.example.com", False, "")
	putStr$unitTest("test.@example.com", False, "")
	putStr$unitTest("test..test@example.com", False, "")
	putStr$unitTest(".test@example.com", False, "")
	putStr$unitTest("test@test@example.com", False, "")
	putStr$unitTest("test@@example.com", False, "")
	putStr$unitTest("-- test --@example.com", False, "No spaces allowed in local part")
	putStr$unitTest("[test]@example.com", False, "Square brackets only allowed within quotes")
	putStr$unitTest("\"test\\test\"@example.com", True, "Any character can be escaped in a quoted string")
	putStr$unitTest("\"test\"test\"@example.com", False, "Quotes cannot be nested")
	putStr$unitTest("()[]\\;:,><@example.com", False, "Disallowed Characters")
	putStr$unitTest("test@.", False, "Dave Child says so")
	putStr$unitTest("test@example.", False, "Dave Child says so")
	putStr$unitTest("test@.org", False, "Dave Child says so")
	putStr$unitTest("test@123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012.com", False, "255 characters is maximum length for domain. This is 256.")
	putStr$unitTest("test@example", False, "Dave Child says so")
	putStr$unitTest("test@[123.123.123.123", False, "Dave Child says so")
	putStr$unitTest("test@123.123.123.123]", False, "Dave Child says so")
	putStr$unitTest("NotAnEmail", False, "Phil Haack says so")
	putStr$unitTest("@NotAnEmail", False, "Phil Haack says so")
	putStr$unitTest("\"test\\\\blah\"@example.com", True, "")
	putStr$unitTest("\"test\\blah\"@example.com", True, "Any character can be escaped in a quoted string")
	putStr$unitTest("\"test\\\rblah\"@example.com", True, "Quoted string specifically excludes carriage returns unless escaped")
	putStr$unitTest("\"test\rblah\"@example.com", False, "Quoted string specifically excludes carriage returns")
	putStr$unitTest("\"test\\\"blah\"@example.com", True, "")
	putStr$unitTest("\"test\"blah\"@example.com", False, "Phil Haack says so")
	putStr$unitTest("customer/department@example.com", True, "")
	putStr$unitTest("_Yosemite.Sam@example.com", True, "")
	putStr$unitTest("~@example.com", True, "")
	putStr$unitTest(".wooly@example.com", False, "Phil Haack says so")
	putStr$unitTest("wo..oly@example.com", False, "Phil Haack says so")
	putStr$unitTest("pootietang.@example.com", False, "Phil Haack says so")
	putStr$unitTest(".@example.com", False, "Phil Haack says so")
	putStr$unitTest("\"Austin@Powers\"@example.com", True, "")
	putStr$unitTest("Ima.Fool@example.com", True, "")
	putStr$unitTest("\"Ima.Fool\"@example.com", True, "")
	putStr$unitTest("\"Ima Fool\"@example.com", True, "")
	putStr$unitTest("Ima Fool@example.com", False, "Phil Haack says so")
	putStr$unitTest("phil.h\\@\\@ck@haacked.com", False, "Escaping can only happen in a quoted string")
	putStr$unitTest("\"first\".\"last\"@example.com", True, "")
	putStr$unitTest("\"first\".middle.\"last\"@example.com", True, "")
	putStr$unitTest("\"first\\\\\"last\"@example.com", False, "Contains an unescaped quote")
	putStr$unitTest("\"first\".last@example.com", True, "obs-local-part form as described in RFC 2822")
	putStr$unitTest("first.\"last\"@example.com", True, "obs-local-part form as described in RFC 2822")
	putStr$unitTest("\"first\".\"middle\".\"last\"@example.com", True, "obs-local-part form as described in RFC 2822")
	putStr$unitTest("\"first.middle\".\"last\"@example.com", True, "obs-local-part form as described in RFC 2822")
	putStr$unitTest("\"first.middle.last\"@example.com", True, "obs-local-part form as described in RFC 2822")
	putStr$unitTest("\"first..last\"@example.com", True, "obs-local-part form as described in RFC 2822")
	putStr$unitTest("foo@[\\1.2.3.4]", False, "RFC 5321 specifies the syntax for address-literal and does not allow escaping")
	putStr$unitTest("\"first\\\\\\\"last\"@example.com", True, "")
	putStr$unitTest("first.\"mid\\dle\".\"last\"@example.com", True, "Backslash can escape anything but must escape something")
	putStr$unitTest("Test.\r\n Folding.\r\n Whitespace@example.com", True, "")
	putStr$unitTest("first.\"\".last@example.com", False, "Contains a zero-length element")
	putStr$unitTest("first\\last@example.com", False, "Unquoted string must be an atom")
	putStr$unitTest("Abc\\@def@example.com", False, "Was incorrectly given as a valid address in the original RFC3696")
	putStr$unitTest("Fred\\ Bloggs@example.com", False, "Was incorrectly given as a valid address in the original RFC3696")
	putStr$unitTest("Joe.\\\\Blow@example.com", False, "Was incorrectly given as a valid address in the original RFC3696")
	putStr$unitTest("first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.567.89]", False, "IPv4 part contains an invalid octet")
	putStr$unitTest("\"test\\\r\n blah\"@example.com", False, "Folding white space can\'t appear within a quoted pair")
	putStr$unitTest("\"test\r\n blah\"@example.com", True, "This is a valid quoted string with folding white space")
	putStr$unitTest("{^c\\@**Dog^}@cartoon.com", False, "This is a throwaway example from Doug Lovell\'s article. Actually it\'s not a valid address.")
	putStr$unitTest("(foo)cal(bar)@(baz)iamcal.com(quux)", True, "A valid address containing comments")
	putStr$unitTest("cal@iamcal(woo).(yay)com", True, "A valid address containing comments")
	putStr$unitTest("\"foo\"(yay)@(hoopla)[1.2.3.4]", False, "Address literal can\'t be commented (RFC5321)")
	putStr$unitTest("cal(woo(yay)hoopla)@iamcal.com", True, "A valid address containing comments")
	putStr$unitTest("cal(foo\\@bar)@iamcal.com", True, "A valid address containing comments")
	putStr$unitTest("cal(foo\\)bar)@iamcal.com", True, "A valid address containing comments and an escaped parenthesis")
	putStr$unitTest("cal(foo(bar)@iamcal.com", False, "Unclosed parenthesis in comment")
	putStr$unitTest("cal(foo)bar)@iamcal.com", False, "Too many closing parentheses")
	putStr$unitTest("cal(foo\\)@iamcal.com", False, "Backslash at end of comment has nothing to escape")
	putStr$unitTest("first().last@example.com", True, "A valid address containing an empty comment")
	putStr$unitTest("first.(\r\n middle\r\n )last@example.com", True, "Comment with folding white space")
	putStr$unitTest("first(12345678901234567890123456789012345678901234567890)last@(1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)example.com", False, "Too long with comments, not too long without")
	putStr$unitTest("first(Welcome to\r\n the (\"wonderful\" (!)) world\r\n of email)@example.com", True, "Silly example from my blog post")
	putStr$unitTest("pete(his account)@silly.test(his host)", True, "Canonical example from RFC5322")
	putStr$unitTest("c@(Chris\'s host.)public.example", True, "Canonical example from RFC5322")
	putStr$unitTest("jdoe@machine(comment).  example", True, "Canonical example from RFC5322")
	putStr$unitTest("1234   @   local(blah)  .machine .example", True, "Canonical example from RFC5322")
	putStr$unitTest("first(middle)last@example.com", False, "Can\'t have a comment or white space except at an element boundary")
	putStr$unitTest("first(abc.def).last@example.com", True, "Comment can contain a dot")
	putStr$unitTest("first(a\"bc.def).last@example.com", True, "Comment can contain double quote")
	putStr$unitTest("first.(\")middle.last(\")@example.com", True, "Comment can contain a quote")
	putStr$unitTest("first(abc(\"def\".ghi).mno)middle(abc(\"def\".ghi).mno).last@(abc(\"def\".ghi).mno)example(abc(\"def\".ghi).mno).(abc(\"def\".ghi).mno)com(abc(\"def\".ghi).mno)", False, "Can\'t have comments or white space except at an element boundary")
	putStr$unitTest("first(abc\\(def)@example.com", True, "Comment can contain quoted-pair")
	putStr$unitTest("first.last@x(1234567890123456789012345678901234567890123456789012345678901234567890).com", True, "Label is longer than 63 octets, but not with comment removed")
	putStr$unitTest("a(a(b(c)d(e(f))g)h(i)j)@example.com", True, "")
	putStr$unitTest("a(a(b(c)d(e(f))g)(h(i)j)@example.com", False, "Braces are not properly matched")
	putStr$unitTest("name.lastname@domain.com", True, "")
	putStr$unitTest(".@", False, "")
	putStr$unitTest("a@b", False, "")
	putStr$unitTest("@bar.com", False, "")
	putStr$unitTest("@@bar.com", False, "")
	putStr$unitTest("a@bar.com", True, "")
	putStr$unitTest("aaa.com", False, "")
	putStr$unitTest("aaa@.com", False, "")
	putStr$unitTest("aaa@.123", False, "")
	putStr$unitTest("aaa@[123.123.123.123]", True, "")
	putStr$unitTest("aaa@[123.123.123.123]a", False, "extra data outside ip")
	putStr$unitTest("aaa@[123.123.123.333]", False, "not a valid IP")
	putStr$unitTest("a@bar.com.", False, "")
	putStr$unitTest("a@bar", False, "")
	putStr$unitTest("a-b@bar.com", True, "")
	putStr$unitTest("+@b.c", True, "TLDs can be any length")
	putStr$unitTest("+@b.com", True, "")
	putStr$unitTest("a@-b.com", False, "")
	putStr$unitTest("a@b-.com", False, "")
	putStr$unitTest("-@..com", False, "")
	putStr$unitTest("-@a..com", False, "")
	putStr$unitTest("a@b.co-foo.uk", True, "")
	putStr$unitTest("\"hello my name is\"@stutter.com", True, "")
	putStr$unitTest("\"Test \\\"Fail\\\" Ing\"@example.com", True, "")
	putStr$unitTest("valid@special.museum", True, "")
	putStr$unitTest("invalid@special.museum-", False, "")
	putStr$unitTest("shaitan@my-domain.thisisminekthx", True, "Disagree with Paul Gregg here")
	putStr$unitTest("test@...........com", False, "......")
	putStr$unitTest("foobar@192.168.0.1", False, "ip need to be []")
	putStr$unitTest("\"Joe\\\\Blow\"@example.com", True, "")
	putStr$unitTest("Invalid \\\n Folding \\\n Whitespace@example.com", False, "This isn\'t FWS so Dominic Sayers says it\'s invalid")
	putStr$unitTest("HM2Kinsists@(that comments are allowed)this.is.ok", True, "")
	putStr$unitTest("user%uucp!path@somehost.edu", True, "")
	putStr$unitTest("\"first(last)\"@example.com", True, "")
	putStr$unitTest(" \r\n (\r\n x \r\n ) \r\n first\r\n ( \r\n x\r\n ) \r\n .\r\n ( \r\n x) \r\n last \r\n (  x \r\n ) \r\n @example.com", True, "")
	putStr$unitTest("test.\r\n \r\n obs@syntax.com", True, "obs-fws allows multiple lines")
	putStr$unitTest("test. \r\n \r\n obs@syntax.com", True, "obs-fws allows multiple lines (test 2: space before break)")
	putStr$unitTest("test.\r\n\r\n obs@syntax.com", False, "obs-fws must have at least one WSP per line")
	putStr$unitTest("\"null \\\0\"@char.com", True, "can have escaped null character")
	putStr$unitTest("\"null \0\"@char.com", False, "cannot have unescaped null character")
	

