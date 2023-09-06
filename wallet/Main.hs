{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Options.Generic

-- import Stellar.Simple

data Options = Trust{account :: Text, asset :: Text} | Pay
    deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
    options <- getRecord "Simple CLI Stellar wallet"
    print (options :: Options)
