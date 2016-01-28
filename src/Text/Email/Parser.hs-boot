module Text.Email.Parser
    ( addrSpec
    )
where

import                Data.Attoparsec.ByteString.Char8 (Parser)

import {-# SOURCE #-} Data.EmailAddress (EmailAddress)

addrSpec :: Parser EmailAddress
