import Test.DocTest

main = doctest
    [ "-isrc"
    , "src/Text/Email/QuasiQuotation.hs"
    , "src/Text/Email/Validate.hs"
    , "-XQuasiQuotes"
    , "-XOverloadedStrings"
    ]
