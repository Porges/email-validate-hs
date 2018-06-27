{-# LANGUAGE TemplateHaskell #-}

module Main where

import Criterion
import Criterion.Main (defaultMain)
import Data.ByteString (ByteString)
import Data.Char (ord, chr)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.XML.Light as X

import Text.Email.Validate (validate) 

testXml :: ByteString
testXml = $(makeRelativeToProject "testdata/isemail/test/tests.xml" >>= embedFile)

parseTestCases :: ByteString -> [Text]
parseTestCases testXml =
    let tests = X.onlyElems (X.parseXML testXml) >>= X.findElements (name "test") in
    map address tests

    where
    child s el = X.findChild (name s) el & fromJust & X.strContent
    address el = T.pack (translateVisibleControls (child "address" el))
    name s = X.QName s Nothing Nothing
    -- the XML has C0 controls mapped to the visible controls, since XML can't
    -- contain them directly:
    translateVisibleControls =
        map (\c -> 
            if c >= '\x2400' && c <= '\x241f'
            then chr (ord c - 0x2400)
            else c)

main :: IO ()
main = do
    let testCases = take 10 (parseTestCases testXml)
    -- force the list
    print $ sum (map T.length testCases)

    defaultMain [
        bgroup "Test cases from tests.xml" $
            flip map testCases $ \testCase -> 
                bench (show testCase) (whnf validate testCase)
        ]

