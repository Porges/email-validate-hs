import Test.DocTest

main = doctest
    [ "-isrc"
    , "src/Text/Email/QuasiQuotation.hs"
    ]
