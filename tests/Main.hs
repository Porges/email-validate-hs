{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (isInfixOf)
import Data.Maybe (Maybe(..), isNothing, fromJust)
import Data.Monoid ((<>))

import Test.Hspec (hspec, context, describe, errorCall, it, parallel, shouldBe, shouldSatisfy)
import Test.QuickCheck (Arbitrary(..), suchThat, property)

import Text.Email.QuasiQuotation (email)
import Text.Email.Validate
    ( EmailAddress
    , canonicalizeEmail
    , domainPart
    , emailAddress
    , localPart
    , isValid
    , toByteString
    , validate
    , unsafeEmailAddress
    )

main :: IO ()
main = hspec $ parallel $ do

    showAndRead
    canonicalization
    exampleTests
    specificFailures
    simpleAccessors
    quasiQuotationTests

canonicalization =
    describe "emailAddress" $ do
        it "is idempotent" $
            property prop_doubleCanonicalize

exampleTests =
    describe "Examples" $ do
        forM_ examples $ \Example{example, exampleValid, exampleWhy, errorContains} -> do
            context (show example ++ (if null exampleWhy then "" else " (" ++ exampleWhy ++ ")")) $ do
                if exampleValid
                then do
                    it "should be valid" $ 
                        isValid example `shouldBe` True

                    it "passes double-canonicalization test" $
                        prop_doubleCanonicalize (fromJust (emailAddress example))

                else do
                    it "should be invalid" $
                        isValid example `shouldBe` False

                    case (errorContains, validate example) of
                        (Just err, Left errMessage) ->
                            it "should have correct error message" $
                                errMessage `shouldSatisfy` (err `isInfixOf`)
                        (_, _) -> return ()

showAndRead =
    describe "show/read instances" $ do

        it "can roundtrip" $
            property prop_showAndReadBack

        it "shows in the same way as ByteString" $
            property prop_showLikeByteString

        it "should fail if read back without a quote" $
            property prop_showAndReadBackWithoutQuoteFails

specificFailures = do
    describe "GitHub issue #12" $ do
        it "is fixed" $
            let (Right em) = validate (BS.pack "\"\"@1") in
            em `shouldBe` read (show em)

    describe "Trailing dot" $ do
        it "is canonicalized" $ 
            canonicalizeEmail "foo@bar.com." `shouldBe` Just "foo@bar.com"

simpleAccessors = do
    describe "localPart" $
        it "extracts local part" $
            localPart (unsafeEmailAddress "local" undefined) `shouldBe` "local"


    describe "domainPart" $
        it "extracts domain part" $
            domainPart (unsafeEmailAddress undefined "domain") `shouldBe` "domain"

quasiQuotationTests =
    describe "QuasiQuoter" $ do
        it "works as expected" $
            [email|local@domain.com|] `shouldBe` unsafeEmailAddress "local" "domain.com"

instance Arbitrary ByteString where
    arbitrary = fmap BS.pack arbitrary

instance Arbitrary EmailAddress where
    arbitrary = do
        local <- suchThat arbitrary (\l -> isEmail l (BS.pack "example.com"))
        domain <- suchThat arbitrary (\d -> isEmail (BS.pack "example") d)
        let (Just result) = emailAddress (makeEmailLike local domain)
        pure result

        where
        isEmail l d = isValid (makeEmailLike l d)
        makeEmailLike l d = BS.concat [l, BS.singleton '@', d]

{- Properties -}

prop_doubleCanonicalize :: EmailAddress -> Bool
prop_doubleCanonicalize email =  Just email == emailAddress (toByteString email)

prop_showLikeByteString :: EmailAddress -> Bool
prop_showLikeByteString email = show (toByteString email) == show email

prop_showAndReadBack :: EmailAddress -> Bool
prop_showAndReadBack email = read (show email) == email

prop_showAndReadBackWithoutQuoteFails :: EmailAddress -> Bool
prop_showAndReadBackWithoutQuoteFails email =
    isNothing (readMaybe (init s)) && isNothing (readMaybe (tail s))
    where
    s = show email
    readMaybe :: String -> Maybe EmailAddress
    readMaybe s = case reads s of
                [(x, "")] -> Just x
                _ -> Nothing

{- Examples -}

data Example = Example
    { example :: ByteString
    , exampleValid :: Bool
    , exampleWhy :: String
    , errorContains :: Maybe String }

valid, invalid :: ByteString -> Example
valid e = Example e True "" Nothing
invalid e = Example e False "" Nothing

why :: Example -> String -> Example
why ex str = ex { exampleWhy = str }

errorShouldContain :: Example -> String -> Example
errorShouldContain ex str = ex { errorContains = Just str }


examples :: [Example]
examples =
    let domain249 = BS.intercalate "." (take 25 (repeat (BS.replicate 9 'x'))) in
    [ valid "first.last@example.com"
    , valid "first.last@example.com." `why` "Dot allowed on end of domain"
    , invalid "local@exam_ple.com" `why` "Underscore not permitted in domain"
    , valid "1234567890123456789012345678901234567890123456789012345678901234@example.com"
    , valid "\"first last\"@example.com" `why` "Contains quoted spaces"
    , valid "\"first\\\"last\"@example.com" `why` "Contains quoted escaped quote"
    , invalid "first\\@last@example.com" `why` "Escaping can only happen within a quoted string"
    , valid "\"first@last\"@example.com" `why` "Contains quoted at-sign"
    , valid "\"first\\\\last\"@example.com" `why` "Contains quoted escaped backslash"
    , valid ("1234@" <> domain249)
        `why` "Maximum length is 254, this is 254 exactly"
    , valid ("1234@" <> domain249 <> ".")
        `why` "Trailing dot doesn't increase length"
    , invalid ("12345@" <> domain249)
        `why` "Maximum length is 254, this is 255"
        `errorShouldContain` "too long"
    , valid "first.last@[12.34.56.78]" `why` "IP address"
    , valid "first.last@[IPv6:::12.34.56.78]" `why` "IPv6 address"
    , valid "first.last@[IPv6:1111:2222:3333::4444:12.34.56.78]"
    , valid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.56.78]"
    , valid "first.last@[IPv6:::1111:2222:3333:4444:5555:6666]"
    , valid "first.last@[IPv6:1111:2222:3333::4444:5555:6666]"
    , valid "first.last@[IPv6:1111:2222:3333:4444:5555:6666::]"
    , valid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888]"
    , valid "first.last@x23456789012345678901234567890123456789012345678901234567890123.example.com"
    , valid "first.last@1xample.com"
    , valid "first.last@123.example.com"
    , invalid "first.last" `why` "no at sign" `errorShouldContain` "at sign"
    , invalid ".first.last@example.com" `why` "Local part starts with a dot"
    , invalid "first.last.@example.com" `why` "Local part ends with a dot"
    , invalid "first..last@example.com" `why` "Local part has consecutive dots"
    , invalid "\"first\"last\"@example.com" `why` "Local part contains unescaped excluded characters"
    , valid "\"first\\last\"@example.com" `why` "Any character can be escaped in a quoted string"
    , invalid "\"\"\"@example.com" `why` "Local part contains unescaped excluded characters"
    , invalid "\"\\\"@example.com" `why` "Local part cannot end with a backslash"
    , invalid "first\\\\@last@example.com" `why` "Local part contains unescaped excluded characters"
    , invalid "first.last@" `why` "No domain"
    , valid "\"Abc\\@def\"@example.com"
    , valid "\"Fred\\ Bloggs\"@example.com"
    , valid "\"Joe.\\\\Blow\"@example.com"
    , valid "\"Abc@def\"@example.com"
    , valid "\"Fred Bloggs\"@example.com"
    , valid "user+mailbox@example.com"
    , valid "customer/department=shipping@example.com"
    , valid "$A12345@example.com"
    , valid "!def!xyz%abc@example.com"
    , valid "_somename@example.com"
    , valid "dclo@us.ibm.com"
    , invalid "abc\\@def@example.com" `why` "This example from RFC3696 was corrected in an erratum"
    , invalid "abc\\\\@example.com" `why` "This example from RFC3696 was corrected in an erratum"
    , valid "peter.piper@example.com"
    , invalid "Doug\\ \\\"Ace\\\"\\ Lovell@example.com" `why` "Escaping can only happen in a quoted string"
    , valid "\"Doug \\\"Ace\\\" L.\"@example.com"
    , invalid "abc@def@example.com" `why` "Doug Lovell says this should fail"
    , invalid "abc\\\\@def@example.com" `why` "Doug Lovell says this should fail"
    , invalid "abc\\@example.com" `why` "Doug Lovell says this should fail"
    , invalid "@example.com" `why` "no local part"
    , invalid "doug@" `why` "no domain part"
    , invalid "\"qu@example.com" `why` "Doug Lovell says this should fail"
    , invalid "ote\"@example.com" `why` "Doug Lovell says this should fail"
    , invalid ".dot@example.com" `why` "Doug Lovell says this should fail"
    , invalid "dot.@example.com" `why` "Doug Lovell says this should fail"
    , invalid "two..dot@example.com" `why` "Doug Lovell says this should fail"
    , invalid "\"Doug \"Ace\" L.\"@example.com" `why` "Doug Lovell says this should fail"
    , invalid "Doug\\ \\\"Ace\\\"\\ L\\.@example.com" `why` "Doug Lovell says this should fail"
    , invalid "hello world@example.com" `why` "Doug Lovell says this should fail"
    , valid "gatsby@f.sc.ot.t.f.i.tzg.era.l.d."
    , valid "test@example.com"
    , valid "TEST@example.com"
    , valid "1234567890@example.com"
    , valid "test+test@example.com"
    , valid "test-test@example.com"
    , valid "t*est@example.com"
    , valid "+1~1+@example.com"
    , valid "{_test_}@example.com"
    , valid "\"[[ test ]]\"@example.com"
    , valid "test.test@example.com"
    , valid "\"test.test\"@example.com"
    , valid "test.\"test\"@example.com" `why` "Obsolete form, but documented in RFC2822"
    , valid "\"test@test\"@example.com"
    , valid "test@123.123.123.x123"
    , valid "test@[123.123.123.123]"
    , valid "test@example.example.com"
    , valid "test@example.example.example.com"
    , invalid "test.example.com"
    , invalid "test.@example.com"
    , invalid "test..test@example.com"
    , invalid ".test@example.com"
    , invalid "test@test@example.com"
    , invalid "test@@example.com"
    , invalid "-- test --@example.com" `why` "No spaces allowed in local part"
    , invalid "[test]@example.com" `why` "Square brackets only allowed within quotes"
    , valid "\"test\\test\"@example.com" `why` "Any character can be escaped in a quoted string"
    , invalid "\"test\"test\"@example.com" `why` "Quotes cannot be nested"
    , invalid "()[]\\;:,><@example.com" `why` "Disallowed Characters"
    , invalid "test@." `why` "Dave Child says so"
    , valid "test@example."
    , invalid "test@.org" `why` "Dave Child says so"
    , invalid "test@[123.123.123.123" `why` "Dave Child says so"
    , invalid "test@123.123.123.123]" `why` "Dave Child says so"
    , invalid "NotAnEmail" `why` "Phil Haack says so"
    , invalid "@NotAnEmail" `why` "Phil Haack says so"
    , valid "\"test\\\\blah\"@example.com"
    , valid "\"test\\blah\"@example.com" `why` "Any character can be escaped in a quoted string"
    , valid "\"test\\\rblah\"@example.com" `why` "Quoted string specifically excludes carriage returns unless escaped"
    , invalid "\"test\rblah\"@example.com" `why` "Quoted string specifically excludes carriage returns"
    , valid "\"test\\\"blah\"@example.com"
    , invalid "\"test\"blah\"@example.com" `why` "Phil Haack says so"
    , valid "customer/department@example.com"
    , valid "_Yosemite.Sam@example.com"
    , valid "~@example.com"
    , invalid ".wooly@example.com" `why` "Phil Haack says so"
    , invalid "wo..oly@example.com" `why` "Phil Haack says so"
    , invalid "pootietang.@example.com" `why` "Phil Haack says so"
    , invalid ".@example.com" `why` "Phil Haack says so"
    , valid "\"Austin@Powers\"@example.com"
    , valid "Ima.Fool@example.com"
    , valid "\"Ima.Fool\"@example.com"
    , valid "\"Ima Fool\"@example.com"
    , invalid "Ima Fool@example.com" `why` "Phil Haack says so"
    , invalid "phil.h\\@\\@ck@haacked.com" `why` "Escaping can only happen in a quoted string"
    , valid "\"first\".\"last\"@example.com"
    , valid "\"first\".middle.\"last\"@example.com"
    , invalid "\"first\\\\\"last\"@example.com" `why` "Contains an unescaped quote"
    , valid "\"first\".last@example.com" `why` "obs-local-part form as described in RFC 2822"
    , valid "first.\"last\"@example.com" `why` "obs-local-part form as described in RFC 2822"
    , valid "\"first\".\"middle\".\"last\"@example.com" `why` "obs-local-part form as described in RFC 2822"
    , valid "\"first.middle\".\"last\"@example.com" `why` "obs-local-part form as described in RFC 2822"
    , valid "\"first.middle.last\"@example.com" `why` "obs-local-part form as described in RFC 2822"
    , valid "\"first..last\"@example.com" `why` "obs-local-part form as described in RFC 2822"
    , invalid "foo@[\\1.2.3.4]" `why` "RFC 5321 specifies the syntax for address-literal and does not allow escaping"
    , valid "\"first\\\\\\\"last\"@example.com"
    , valid "first.\"mid\\dle\".\"last\"@example.com" `why` "Backslash can escape anything but must escape something"
    , valid "Test.\r\n Folding.\r\n Whitespace@example.com"
    , invalid "first\\last@example.com" `why` "Unquoted string must be an atom"
    , invalid "Abc\\@def@example.com" `why` "Was incorrectly given as a valid address in the original RFC3696"
    , invalid "Fred\\ Bloggs@example.com" `why` "Was incorrectly given as a valid address in the original RFC3696"
    , invalid "Joe.\\\\Blow@example.com" `why` "Was incorrectly given as a valid address in the original RFC3696"
    , invalid "\"test\\\r\n blah\"@example.com" `why` "Folding white space can\'t appear within a quoted pair"
    , valid "\"test\r\n blah\"@example.com" `why` "This is a valid quoted string with folding white space"
    , invalid "{^c\\@**Dog^}@cartoon.com" `why` "This is a throwaway example from Doug Lovell\'s article. Actually it\'s not a valid address."
    , valid "(foo)cal(bar)@(baz)iamcal.com(quux)" `why` "A valid address containing comments"
    , valid "cal@iamcal(woo).(yay)com" `why` "A valid address containing comments"
    , valid "cal(woo(yay)hoopla)@iamcal.com" `why` "A valid address containing comments"
    , valid "cal(foo\\@bar)@iamcal.com" `why` "A valid address containing comments"
    , valid "cal(foo\\)bar)@iamcal.com" `why` "A valid address containing comments and an escaped parenthesis"
    , invalid "cal(foo(bar)@iamcal.com" `why` "Unclosed parenthesis in comment"
    , invalid "cal(foo)bar)@iamcal.com" `why` "Too many closing parentheses"
    , invalid "cal(foo\\)@iamcal.com" `why` "Backslash at end of comment has nothing to escape"
    , valid "first().last@example.com" `why` "A valid address containing an empty comment"
    , valid "first.(\r\n middle\r\n )last@example.com" `why` "Comment with folding white space"
    , invalid "first(12345678901234567890123456789012345678901234567890)last@(1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890)example.com" `why` "Too long with comments, not too long without"
    , valid "first(Welcome to\r\n the (\"wonderful\" (!)) world\r\n of email)@example.com" `why` "Silly example from my blog post"
    , valid "pete(his account)@silly.test(his host)" `why` "Canonical example from RFC5322"
    , valid "c@(Chris\'s host.)public.example" `why` "Canonical example from RFC5322"
    , valid "jdoe@machine(comment).  example" `why` "Canonical example from RFC5322"
    , valid "1234   @   local(blah)  .machine .example" `why` "Canonical example from RFC5322"
    , invalid "first(middle)last@example.com" `why` "Can\'t have a comment or white space except at an element boundary"
    , valid "first(abc.def).last@example.com" `why` "Comment can contain a dot"
    , valid "first(a\"bc.def).last@example.com" `why` "Comment can contain double quote"
    , valid "first.(\")middle.last(\")@example.com" `why` "Comment can contain a quote"
    , invalid "first(abc(\"def\".ghi).mno)middle(abc(\"def\".ghi).mno).last@(abc(\"def\".ghi).mno)example(abc(\"def\".ghi).mno).(abc(\"def\".ghi).mno)com(abc(\"def\".ghi).mno)" `why` "Can\'t have comments or white space except at an element boundary"
    , valid "first(abc\\(def)@example.com" `why` "Comment can contain quoted-pair"
    , valid "first.last@x(1234567890123456789012345678901234567890123456789012345678901234567890).com" `why` "Label is longer than 63 octets, but not with comment removed"
    , valid "a(a(b(c)d(e(f))g)h(i)j)@example.com"
    , invalid "a(a(b(c)d(e(f))g)(h(i)j)@example.com" `why` "Braces are not properly matched"
    , valid "name.lastname@domain.com"
    , invalid ".@"
    , invalid "@bar.com"
    , invalid "@@bar.com"
    , valid "a@bar.com"
    , invalid "aaa.com"
    , invalid "aaa@.com"
    , invalid "aaa@.123"
    , valid "aaa@[123.123.123.123]"
    , invalid "aaa@[123.123.123.123]a" `why` "extra data outside ip"
    , valid "a@bar.com."
    , valid "a-b@bar.com"
    , valid "+@b.c" `why` "TLDs can be any length"
    , valid "+@b.com"
    , invalid "-@..com"
    , invalid "-@a..com"
    , valid "a@b.co-foo.uk"
    , valid "\"hello my name is\"@stutter.com"
    , valid "\"Test \\\"Fail\\\" Ing\"@example.com"
    , valid "valid@special.museum"
    , valid "shaitan@my-domain.thisisminekthx" `why` "Disagree with Paul Gregg here"
    , invalid "test@...........com" `why` "......"
    , valid "\"Joe\\\\Blow\"@example.com"
    , invalid "Invalid \\\n Folding \\\n Whitespace@example.com" `why` "This isn\'t FWS so Dominic Sayers says it\'s invalid"
    , valid "HM2Kinsists@(that comments are allowed)this.is.ok"
    , valid "user%uucp!path@somehost.edu"
    , valid "\"first(last)\"@example.com"
    , valid " \r\n (\r\n x \r\n ) \r\n first\r\n ( \r\n x\r\n ) \r\n .\r\n ( \r\n x) \r\n last \r\n (  x \r\n ) \r\n @example.com"
    , valid "test.\r\n \r\n obs@syntax.com" `why` "obs-fws allows multiple lines"
    , valid "test. \r\n \r\n obs@syntax.com" `why` "obs-fws allows multiple lines (test 2: space before break)"
    , invalid "test.\r\n\r\n obs@syntax.com" `why` "obs-fws must have at least one WSP per line"
    , valid "\"null \\\0\"@char.com" `why` "can have escaped null character"
    , invalid "\"null \0\"@char.com" `why` "cannot have unescaped null character"
    -- items below here are invalid according to other RFCs (or opinions)
    --, invalid "\"\"@example.com" `why` "Local part is effectively empty"
    --, invalid "foobar@192.168.0.1" `why` "ip need to be []"
    --, invalid "first.last@[.12.34.56.78]" `why` "Only char that can precede IPv4 address is \':\'"
    --, invalid "first.last@[12.34.56.789]" `why` "Can\'t be interpreted as IPv4 so IPv6 tag is missing"
    --, invalid "first.last@[::12.34.56.78]" `why` "IPv6 tag is missing"
    --, invalid "first.last@[IPv5:::12.34.56.78]" `why` "IPv6 tag is wrong"
    --, invalid "first.last@[IPv6:1111:2222:3333::4444:5555:12.34.56.78]" `why` "Too many IPv6 groups (4 max)"
    --, invalid "first.last@[IPv6:1111:2222:3333:4444:5555:12.34.56.78]" `why` "Not enough IPv6 groups"
    --, invalid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:12.34.56.78]" `why` "Too many IPv6 groups (6 max)"
    --, invalid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777]" `why` "Not enough IPv6 groups"
    --, invalid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:7777:8888:9999]" `why` "Too many IPv6 groups (8 max)"
    --, invalid "first.last@[IPv6:1111:2222::3333::4444:5555:6666]" `why` "Too many \'::\' (can be none or one)"
    --, invalid "first.last@[IPv6:1111:2222:3333::4444:5555:6666:7777]" `why` "Too many IPv6 groups (6 max)"
    --, invalid "first.last@[IPv6:1111:2222:333x::4444:5555]" `why` "x is not valid in an IPv6 address"
    --, invalid "first.last@[IPv6:1111:2222:33333::4444:5555]" `why` "33333 is not a valid group in an IPv6 address"
    --, invalid "first.last@example.123" `why` "TLD can\'t be all digits"
    --, invalid "aaa@[123.123.123.333]" `why` "not a valid IP"
    --, invalid "first.last@[IPv6:1111:2222:3333:4444:5555:6666:12.34.567.89]" `why` "IPv4 part contains an invalid octet"
    , valid "a@b"
    , valid "a@bar"
    , invalid "invalid@special.museum-" `why` "domain can't end with hyphen"
    , invalid "a@-b.com" `why` "domain can't start with hyphen"
    , invalid "a@b-.com" `why` "domain label can't end with hyphen"
    --, invalid "\"foo\"(yay)@(hoopla)[1.2.3.4]" `why` "Address literal can\'t be commented (RFC5321)"
    --, invalid "first.\"\".last@example.com" `why` "Contains a zero-length element"
    --, invalid "test@example" `why` "Dave Child says so"
    , invalid (BS.replicate 65 'x' <> "@x") `why` "local-part longer than 64 octets" `errorShouldContain` "too long"
    , invalid "x@x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456789.x23456" `why` "Domain exceeds 255 chars"
    , invalid "test@123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012.com" `why` "255 characters is maximum length for domain. This is 256."
    , invalid "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.1234.example.com" `why` "Entire address is longer than 254 characters (this is 257)"
    , invalid "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.123.example.com" `why` "Entire address is longer than 254 characters (this is 256)"
    , invalid "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12.example.com" `why` "Entire address is longer than 254 characters (this is 255)"
    , valid "123456789012345678901234567890123456789012345678901234567890@12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.12345678901234567890123456789012345678901234567890123456789.1.example.com" `why` "Entire address is 254 characters"
    --, invalid "test@123.123.123.123" `why` "Top Level Domain won\'t be all-numeric (see RFC3696 Section 2). I disagree with Dave Child on this one."
    , invalid "first.last@x234567890123456789012345678901234567890123456789012345678901234.example.com" `why` "Label can\'t be longer than 63 octets"
    --, invalid "first.last@com" `why` "Mail host must be second- or lower level"
    , invalid "first.last@e.-xample.com" `why` "Label can\'t begin with a hyphen"
    , invalid "first.last@exampl-.e.com" `why` "Label can\'t end with a hyphen"
    , valid "\195\188s\195\169r@example.com" `why` "UTF8 pairs are valid atom text"
    , valid "\"\195\188s\195\169r\"@example.com" `why` "UTF8 pairs are valid quoted text"
    , valid "\"\\\195\188s\\\195\169r\"@example.com" `why` "UTF8 pairs are valid quoted pairs"
    , valid "(comm\196\155nt)user@example.com" `why` "UTF8 pairs are valid comment text"
    , valid "us\226\136\138\226\132\157@example.com" `why` "UTF8 triples are valid atom text"
    , valid "\"us\226\136\138\226\132\157\"@example.com" `why` "UTF8 triples are valid quoted text"
    , valid "\"us\\\226\136\138\\\226\132\157\"@example.com" `why` "UTF8 triples are valid quoted pairs"
    , valid "(comm\226\147\148nt)user@example.com" `why` "UTF8 triples are valid comment text"
    , valid "\240\159\150\133@example.com" `why` "UTF8 quads are valid atom text"
    , valid "\"\240\159\150\133\"@example.com" `why` "UTF8 quads are valid quoted text"
    , valid "\"\\\240\159\150\133\"@example.com" `why` "UTF8 quads are valid quoted pairs"
    , valid "(\240\159\150\133)user@example.com" `why` "UTF8 quads are valid comment text"
    ]
