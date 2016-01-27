{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Text.Domain.Parser
    ( domainParser
    )
where

import Control.Applicative
import Control.Monad (void, guard)

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

import Data.Attoparsec.ByteString.Char8


domainParser :: Parser ByteString
domainParser = do
    domain <- fst <$> match (label `sepBy1` char '.' >> optional (char '.'))

    -- trim off the excess '.' if it is there
    let trimmed =
            case BS.last domain of
                '.' -> BS.init domain
                _ -> domain

    -- domain name must be no greater than 253 chars
    guard (BS.length trimmed <= 253)
    return trimmed

label :: Parser ByteString
label = do
    lbl <- fst <$> match (alphaNum >> skipWhile isAlphaNumHyphen)

    -- label must be no greater than 63 chars and cannot end with '-'
    guard (BS.length lbl <= 63 && BS.last lbl /= '-')
    return lbl

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum
    where isAlphaNum x = isDigit x || isAlpha_ascii x

isAlphaNumHyphen :: Char -> Bool
isAlphaNumHyphen x = isDigit x || isAlpha_ascii x || x == '-'
