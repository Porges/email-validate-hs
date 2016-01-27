{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Email.Validate
import Test.HUnit

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
        testGroup "EmailAddress Show/Read instances" [
                testProperty "showLikeByteString" prop_showLikeByteString,
                testProperty "showAndReadBackWithoutQuoteFails" prop_showAndReadBackWithoutQuoteFails,
                testProperty "showAndReadBack" prop_showAndReadBack
                ],

        testGroup "QuickCheck Text.Email.Validate" [
                testProperty "doubleCanonicalize" prop_doubleCanonicalize
                ],

        testGroup "Unit tests Text.Email.Validate" $ flip concatMap units
            (\(email, valid, reason) ->
                    [
                    testCase ("doubleCanonicalize test (" ++ reason ++ "): " ++ show email)
                        (True @=? case emailAddress email of { Nothing -> True; Just ok -> prop_doubleCanonicalize ok }),
                    testCase ("validity test (" ++ reason ++ "): " ++ show email)
                        (valid @=? isValid email)
                    ]),

        testGroup "Specifics" [
            testCase "Issue #12" (let (Right em) = validate (BS.pack "\"\"@1") in em @=? read (show em)),
            testCase "Check canonicalization of trailing dot" (canonicalizeEmail "foo@bar.com." @=? Just "foo@bar.com")
            ]
       ]

instance Arbitrary ByteString where
    arbitrary = fmap BS.pack arbitrary

instance Arbitrary EmailAddress where
    arbitrary = do
        local <- suchThat arbitrary (\x -> isEmail x (BS.pack "example.com"))
        domain <- suchThat arbitrary (isEmail (BS.pack "example"))
        let email = makeEmailLike local domain
        let (Just result) = emailAddress email
        return result

isEmail :: ByteString -> ByteString -> Bool
isEmail l d = isValid (makeEmailLike l d)

makeEmailLike :: ByteString -> ByteString -> ByteString
makeEmailLike l d = BS.concat [l, BS.singleton '@', d]

prop_doubleCanonicalize :: EmailAddress -> Bool
prop_doubleCanonicalize email =  Just email == emailAddress (toByteString email)

prop_showLikeByteString :: EmailAddress -> Bool
prop_showLikeByteString email = show (toByteString email) == show email

prop_showAndReadBack :: EmailAddress -> Bool
prop_showAndReadBack email = read (show email) == email

readMaybe :: String -> Maybe EmailAddress
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

prop_showAndReadBackWithoutQuoteFails :: EmailAddress -> Bool
prop_showAndReadBackWithoutQuoteFails email =
    readMaybe (init s) == Nothing &&
    readMaybe (tail s) == Nothing
    where s = show email

--unitTest (x, y, z) = if not (isValid (BS.pack x) == y) then "" else (x ++" became "++ (case emailAddress (BS.pack x) of {Nothing -> "fail"; Just em -> show em}) ++": Should be "++show y ++", got "++show (not y)++"\n\t"++z++"\n")

units :: [(ByteString, Bool, String)]
units =
    [ ("first.last@example.com", True, "")
    , ("first.last@example.com.", True, "Dot allowed on end of domain")
    , ("local@exam_ple.com", False, "Underscore not permitted in domain")
    , ("1234567890123456789012345678901234567890123456789012345678901234@example.com", True, "")
    , ("\"first last\"@example.com", True, "")
    , ("\"first\\\"last\"@example.com", True, "")
    , ("first\\@last@example.com", False, "Escaping can only happen within a quoted string")
    , ("\"first@last\"@example.com", True, "")
    , ("\"first\\\\last\"@example.com", True, "")
    , ("x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23", True, "Max length is 253")
    , ("x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23.", True, "Trailing dot doesn't increase length")
    , ("x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x234", False, "Max length is 253")
    , ("123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.123456789012345678901234567890123456789012345678901234567890123.example.com", True, "")
    , ("first.last@[12.34.56.78]", True, "")
    , ("first.last@[IPv6:::12.34.56.78]", True, "")
    , ("first.last@[IPv6:1111:2222:3333::4444:12.34.56.78]", True, "")
    , ("first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.56.78]", True, "")
    , ("first.last@[IPv6:::1111:2222:3333:4444:5555:6666]", True, "")
    , ("first.last@[IPv6:1111:2222:3333::4444:5555:6666]", True, "")
    , ("first.last@[IPv6:1111:2222:3333:4444:5555:6666::]", True, "")
    , ("first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]", True, "")
    , ("first.last@x23456789012345678901234567890123456789012345678901234567890123.example.com", True, "")
    , ("first.last@1xample.com", True, "")
    , ("first.last@123.example.com", True, "")
    , ("first.last", False, "No @")
    , (".first.last@example.com", False, "Local part starts with a dot")
    , ("first.last.@example.com", False, "Local part ends with a dot")
    , ("first..last@example.com", False, "Local part has consecutive dots")
    , ("\"first\"last\"@example.com", False, "Local part contains unescaped excluded characters")
    , ("\"first\\last\"@example.com", True, "Any character can be escaped in a quoted string")
    , ("\"\"\"@example.com", False, "Local part contains unescaped excluded characters")
    , ("\"\\\"@example.com", False, "Local part cannot end with a backslash")
    , ("first\\\\@last@example.com", False, "Local part contains unescaped excluded characters")
    , ("first.last@", False, "No domain")
    , ("\"Abc\\@def\"@example.com", True, "")
    , ("\"Fred\\ Bloggs\"@example.com", True, "")
    , ("\"Joe.\\\\Blow\"@example.com", True, "")
    , ("\"Abc@def\"@example.com", True, "")
    , ("\"Fred Bloggs\"@example.com", True, "")
    , ("user+mailbox@example.com", True, "")
    , ("customer/department=shipping@example.com", True, "")
    , ("$A12345@example.com", True, "")
    , ("!def!xyz%abc@example.com", True, "")
    , ("_somename@example.com", True, "")
    , ("dclo@us.ibm.com", True, "")
    , ("abc\\@def@example.com", False, "This example from RFC3696 was corrected in an erratum")
    , ("abc\\\\@example.com", False, "This example from RFC3696 was corrected in an erratum")
    , ("peter.piper@example.com", True, "")
    , ("Doug\\ \\\"Ace\\\"\\ Lovell@example.com", False, "Escaping can only happen in a quoted string")
    , ("\"Doug \\\"Ace\\\" L.\"@example.com", True, "")
    , ("abc@def@example.com", False, "Doug Lovell says this should fail")
    , ("abc\\\\@def@example.com", False, "Doug Lovell says this should fail")
    , ("abc\\@example.com", False, "Doug Lovell says this should fail")
    , ("@example.com", False, "No local part")
    , ("doug@", False, "Doug Lovell says this should fail")
    , ("\"qu@example.com", False, "Doug Lovell says this should fail")
    , ("ote\"@example.com", False, "Doug Lovell says this should fail")
    , (".dot@example.com", False, "Doug Lovell says this should fail")
    , ("dot.@example.com", False, "Doug Lovell says this should fail")
    , ("two..dot@example.com", False, "Doug Lovell says this should fail")
    , ("\"Doug \"Ace\" L.\"@example.com", False, "Doug Lovell says this should fail")
    , ("Doug\\ \\\"Ace\\\"\\ L\\.@example.com", False, "Doug Lovell says this should fail")
    , ("hello world@example.com", False, "Doug Lovell says this should fail")
    , ("gatsby@f.sc.ot.t.f.i.tzg.era.l.d.", True, "")
    , ("test@example.com", True, "")
    , ("TEST@example.com", True, "")
    , ("1234567890@example.com", True, "")
    , ("test+test@example.com", True, "")
    , ("test-test@example.com", True, "")
    , ("t*est@example.com", True, "")
    , ("+1~1+@example.com", True, "")
    , ("{_test_}@example.com", True, "")
    , ("\"[[ test ]]\"@example.com", True, "")
    , ("test.test@example.com", True, "")
    , ("\"test.test\"@example.com", True, "")
    , ("test.\"test\"@example.com", True, "Obsolete form, but documented in RFC2822")
    , ("\"test@test\"@example.com", True, "")
    , ("test@123.123.123.x123", True, "")
    , ("test@[123.123.123.123]", True, "")
    , ("test@example.example.com", True, "")
    , ("test@example.example.example.com", True, "")
    , ("test.example.com", False, "")
    , ("test.@example.com", False, "")
    , ("test..test@example.com", False, "")
    , (".test@example.com", False, "")
    , ("test@test@example.com", False, "")
    , ("test@@example.com", False, "")
    , ("-- test --@example.com", False, "No spaces allowed in local part")
    , ("[test]@example.com", False, "Square brackets only allowed within quotes")
    , ("\"test\\test\"@example.com", True, "Any character can be escaped in a quoted string")
    , ("\"test\"test\"@example.com", False, "Quotes cannot be nested")
    , ("()[]\\;:,><@example.com", False, "Disallowed Characters")
    , ("test@.", False, "Dave Child says so")
    , ("test@example.", True, "")
    , ("test@.org", False, "Dave Child says so")
    , ("test@[123.123.123.123", False, "Dave Child says so")
    , ("test@123.123.123.123]", False, "Dave Child says so")
    , ("NotAnEmail", False, "Phil Haack says so")
    , ("@NotAnEmail", False, "Phil Haack says so")
    , ("\"test\\\\blah\"@example.com", True, "")
    , ("\"test\\blah\"@example.com", True, "Any character can be escaped in a quoted string")
    , ("\"test\\\rblah\"@example.com", True, "Quoted string specifically excludes carriage returns unless escaped")
    , ("\"test\rblah\"@example.com", False, "Quoted string specifically excludes carriage returns")
    , ("\"test\\\"blah\"@example.com", True, "")
    , ("\"test\"blah\"@example.com", False, "Phil Haack says so")
    , ("customer/department@example.com", True, "")
    , ("_Yosemite.Sam@example.com", True, "")
    , ("~@example.com", True, "")
    , (".wooly@example.com", False, "Phil Haack says so")
    , ("wo..oly@example.com", False, "Phil Haack says so")
    , ("pootietang.@example.com", False, "Phil Haack says so")
    , (".@example.com", False, "Phil Haack says so")
    , ("\"Austin@Powers\"@example.com", True, "")
    , ("Ima.Fool@example.com", True, "")
    , ("\"Ima.Fool\"@example.com", True, "")
    , ("\"Ima Fool\"@example.com", True, "")
    , ("Ima Fool@example.com", False, "Phil Haack says so")
    , ("phil.h\\@\\@ck@haacked.com", False, "Escaping can only happen in a quoted string")
    , ("\"first\".\"last\"@example.com", True, "")
    , ("\"first\".middle.\"last\"@example.com", True, "")
    , ("\"first\\\\\"last\"@example.com", False, "Contains an unescaped quote")
    , ("\"first\".last@example.com", True, "obs-local-part form as described in RFC 2822")
    , ("first.\"last\"@example.com", True, "obs-local-part form as described in RFC 2822")
    , ("\"first\".\"middle\".\"last\"@example.com", True, "obs-local-part form as described in RFC 2822")
    , ("\"first.middle\".\"last\"@example.com", True, "obs-local-part form as described in RFC 2822")
    , ("\"first.middle.last\"@example.com", True, "obs-local-part form as described in RFC 2822")
    , ("\"first..last\"@example.com", True, "obs-local-part form as described in RFC 2822")
    , ("foo@[\\1.2.3.4]", False, "RFC 5321 specifies the syntax for address-literal and does not allow escaping")
    , ("\"first\\\\\\\"last\"@example.com", True, "")
    , ("first.\"mid\\dle\".\"last\"@example.com", True, "Backslash can escape anything but must escape something")
    , ("Test.\r\n Folding.\r\n Whitespace@example.com", True, "")
    , ("first\\last@example.com", False, "Unquoted string must be an atom")
    , ("Abc\\@def@example.com", False, "Was incorrectly given as a valid address in the original RFC3696")
    , ("Fred\\ Bloggs@example.com", False, "Was incorrectly given as a valid address in the original RFC3696")
    , ("Joe.\\\\Blow@example.com", False, "Was incorrectly given as a valid address in the original RFC3696")
    , ("\"test\\\r\n blah\"@example.com", False, "Folding white space can\'t appear within a quoted pair")
    , ("\"test\r\n blah\"@example.com", True, "This is a valid quoted string with folding white space")
    , ("{^c\\@**Dog^}@cartoon.com", False, "This is a throwaway example from Doug Lovell\'s article. Actually it\'s not a valid address.")
    , ("(foo)cal(bar)@(baz)iamcal.com(quux)", True, "A valid address containing comments")
    , ("cal@iamcal(woo).(yay)com", True, "A valid address containing comments")
    , ("cal(woo(yay)hoopla)@iamcal.com", True, "A valid address containing comments")
    , ("cal(foo\\@bar)@iamcal.com", True, "A valid address containing comments")
    , ("cal(foo\\)bar)@iamcal.com", True, "A valid address containing comments and an escaped parenthesis")
    , ("cal(foo(bar)@iamcal.com", False, "Unclosed parenthesis in comment")
    , ("cal(foo)bar)@iamcal.com", False, "Too many closing parentheses")
    , ("cal(foo\\)@iamcal.com", False, "Backslash at end of comment has nothing to escape")
    , ("first().last@example.com", True, "A valid address containing an empty comment")
    , ("first.(\r\n middle\r\n )last@example.com", True, "Comment with folding white space")
    , ("first(12345678901234567890123456789012345678901234567890)last@(1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)example.com", False, "Too long with comments, not too long without")
    , ("first(Welcome to\r\n the (\"wonderful\" (!)) world\r\n of email)@example.com", True, "Silly example from my blog post")
    , ("pete(his account)@silly.test(his host)", True, "Canonical example from RFC5322")
    , ("c@(Chris\'s host.)public.example", True, "Canonical example from RFC5322")
    , ("jdoe@machine(comment).  example", True, "Canonical example from RFC5322")
    , ("1234   @   local(blah)  .machine .example", True, "Canonical example from RFC5322")
    , ("first(middle)last@example.com", False, "Can\'t have a comment or white space except at an element boundary")
    , ("first(abc.def).last@example.com", True, "Comment can contain a dot")
    , ("first(a\"bc.def).last@example.com", True, "Comment can contain double quote")
    , ("first.(\")middle.last(\")@example.com", True, "Comment can contain a quote")
    , ("first(abc(\"def\".ghi).mno)middle(abc(\"def\".ghi).mno).last@(abc(\"def\".ghi).mno)example(abc(\"def\".ghi).mno).(abc(\"def\".ghi).mno)com(abc(\"def\".ghi).mno)", False, "Can\'t have comments or white space except at an element boundary")
    , ("first(abc\\(def)@example.com", True, "Comment can contain quoted-pair")
    , ("first.last@x(1234567890123456789012345678901234567890123456789012345678901234567890).com", True, "Label is longer than 63 octets, but not with comment removed")
    , ("a(a(b(c)d(e(f))g)h(i)j)@example.com", True, "")
    , ("a(a(b(c)d(e(f))g)(h(i)j)@example.com", False, "Braces are not properly matched")
    , ("name.lastname@domain.com", True, "")
    , (".@", False, "")
    , ("@bar.com", False, "")
    , ("@@bar.com", False, "")
    , ("a@bar.com", True, "")
    , ("aaa.com", False, "")
    , ("aaa@.com", False, "")
    , ("aaa@.123", False, "")
    , ("aaa@[123.123.123.123]", True, "")
    , ("aaa@[123.123.123.123]a", False, "extra data outside ip")
    , ("a@bar.com.", True, "")
    , ("a-b@bar.com", True, "")
    , ("+@b.c", True, "TLDs can be any length")
    , ("+@b.com", True, "")
    , ("-@..com", False, "")
    , ("-@a..com", False, "")
    , ("a@b.co-foo.uk", True, "")
    , ("\"hello my name is\"@stutter.com", True, "")
    , ("\"Test \\\"Fail\\\" Ing\"@example.com", True, "")
    , ("valid@special.museum", True, "")
    , ("shaitan@my-domain.thisisminekthx", True, "Disagree with Paul Gregg here")
    , ("test@...........com", False, "......")
    , ("\"Joe\\\\Blow\"@example.com", True, "")
    , ("Invalid \\\n Folding \\\n Whitespace@example.com", False, "This isn\'t FWS so Dominic Sayers says it\'s invalid")
    , ("HM2Kinsists@(that comments are allowed)this.is.ok", True, "")
    , ("user%uucp!path@somehost.edu", True, "")
    , ("\"first(last)\"@example.com", True, "")
    , (" \r\n (\r\n x \r\n ) \r\n first\r\n ( \r\n x\r\n ) \r\n .\r\n ( \r\n x) \r\n last \r\n (  x \r\n ) \r\n @example.com", True, "")
    , ("test.\r\n \r\n obs@syntax.com", True, "obs-fws allows multiple lines")
    , ("test. \r\n \r\n obs@syntax.com", True, "obs-fws allows multiple lines (test 2: space before break)")
    , ("test.\r\n\r\n obs@syntax.com", False, "obs-fws must have at least one WSP per line")
    , ("\"null \\\0\"@char.com", True, "can have escaped null character")
    , ("\"null \0\"@char.com", False, "cannot have unescaped null character")
    -- items below here are invalid according to other RFCs (or opinions)
    --, ("\"\"@example.com", False, "Local part is effectively empty")
    --, ("foobar@192.168.0.1", False, "ip need to be []")
    --, ("first.last@[.12.34.56.78]", False, "Only char that can precede IPv4 address is \':\'")
    --, ("first.last@[12.34.56.789]", False, "Can\'t be interpreted as IPv4 so IPv6 tag is missing")
    --, ("first.last@[::12.34.56.78]", False, "IPv6 tag is missing")
    --, ("first.last@[IPv5:::12.34.56.78]", False, "IPv6 tag is wrong")
    --, ("first.last@[IPv6:1111:2222:3333::4444:5555:12.34.56.78]", False, "Too many IPv6 groups (4 max)")
    --, ("first.last@[IPv6:1111:2222:3333:4444:5555:12.34.56.78]", False, "Not enough IPv6 groups")
    --, ("first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:12.34.56.78]", False, "Too many IPv6 groups (6 max)")
    --, ("first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777]", False, "Not enough IPv6 groups")
    --, ("first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888:9999]", False, "Too many IPv6 groups (8 max)")
    --, ("first.last@[IPv6:1111:2222::3333::4444:5555:6666]", False, "Too many \'::\' (can be none or one)")
    --, ("first.last@[IPv6:1111:2222:3333::4444:5555:6666:7777]", False, "Too many IPv6 groups (6 max)")
    --, ("first.last@[IPv6:1111:2222:333x::4444:5555]", False, "x is not valid in an IPv6 address")
    --, ("first.last@[IPv6:1111:2222:33333::4444:5555]", False, "33333 is not a valid group in an IPv6 address")
    --, ("first.last@example.123", False, "TLD can\'t be all digits")
    --, ("aaa@[123.123.123.333]", False, "not a valid IP")
    --, ("first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.567.89]", False, "IPv4 part contains an invalid octet")
    --, ("a@b", False, "")
    --, ("a@bar", False, "")
    , ("invalid@special.museum-", False, "")
    , ("a@-b.com", False, "")
    , ("a@b-.com", False, "")
    --, ("\"foo\"(yay)@(hoopla)[1.2.3.4]", False, "Address literal can\'t be commented (RFC5321)")
    --, ("first.\"\".last@example.com", False, "Contains a zero-length element")
    --, ("test@example", False, "Dave Child says so")
    --, ("12345678901234567890123456789012345678901234567890123456789012345@example.com", False, "Local part more than 64 characters")
    , ("x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456", False, "Domain exceeds 255 chars")
    , ("test@123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012.com", False, "255 characters is maximum length for domain. This is 256.")
    --, ("123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.1234.example.com", False, "Entire address is longer than 256 characters")
    --, ("test@123.123.123.123", False, "Top Level Domain won\'t be all-numeric (see RFC3696 Section 2). I disagree with Dave Child on this one.")
    , ("first.last@x234567890123456789012345678901234567890123456789012345678901234.example.com", False, "Label can\'t be longer than 63 octets")
    --, ("first.last@com", False, "Mail host must be second- or lower level")
    , ("first.last@-xample.com", False, "Label can\'t begin with a hyphen")
    , ("first.last@exampl-.com", False, "Label can\'t end with a hyphen")
    ]
