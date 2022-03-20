{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

module Text.Email.QuasiQuotation
    ( email
    ) where

import qualified Data.ByteString.Char8 as BS8

import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Text.Email.Validate (validate, localPart, domainPart, unsafeEmailAddress)

-- | A QuasiQuoter for email addresses. 
--
-- Use it like this (requires `QuasiQuotes` to be enabled):
-- 
-- >>> [email|someone@example.com|]
-- "someone@example.com"
email :: QuasiQuoter
email = QuasiQuoter
    { quoteExp = quoteEmail emailToExp
    , quotePat = error "email is not supported as a pattern"
    , quoteDec = error "email is not supported at top-level"
    , quoteType = error "email is not supported as a type"
    }
    where

    quoteEmail p s = 
        case validate (BS8.pack s) of 
            Left err -> error ("Invalid quasi-quoted email address: " ++ err) 
            Right e -> p e

    emailToExp e = 
        let lp = BS8.unpack (localPart e) in
        let dp = BS8.unpack (domainPart e) in
        [| unsafeEmailAddress (BS8.pack lp) (BS8.pack dp) |]
