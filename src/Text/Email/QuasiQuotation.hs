{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
#endif

module Text.Email.QuasiQuotation
    ( email
    ) where

import qualified Data.Text as T
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Text.Email.Validate (validate, toText, unsafeEmailAddress)

-- | A QuasiQuoter for email addresses. 
--
-- Use it like this:
-- 
-- >>> :set -XQuasiQuotes
-- >>> [email|someone@example.com|]
-- "someone@example.com"
email :: QuasiQuoter
email = QuasiQuoter
    { quoteExp = quoteEmail
    , quotePat = error "email is not supported as a pattern"
    , quoteDec = error "email is not supported at top-level"
    , quoteType = error "email is not supported as a type"
    }
    where

    quoteEmail s = 
        case validate (T.pack s) of 
            Left err -> error ("Invalid quasi-quoted email address: " ++ err) 
            Right e ->
                let emailString = T.unpack (toText e) in
                [| unsafeEmailAddress (T.pack emailString) |]
