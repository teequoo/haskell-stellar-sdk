{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Network.Stellar.Horizon
    ( HorizonServer
    , publicHorizon
    , testHorizon
    , httpServer
    , httpsServer
    )
where

import Data.Text
import Network.HTTP.Req (Url, Scheme(..), http, https)

type HorizonServer (scheme :: Scheme) = Url scheme

publicHorizon :: HorizonServer 'Https
publicHorizon = httpsServer "horizon.stellar.org"
testHorizon :: HorizonServer 'Https
testHorizon = httpsServer "horizon-testnet.stellar.org"

httpServer :: Text -> HorizonServer 'Http
httpServer url = http url

httpsServer :: Text -> HorizonServer 'Https
httpsServer url = https url
