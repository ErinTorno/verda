module Verda.App
    ( App(..)
    , makeApp
    , makeAppWith
    , start
    , updateWindowConfig
    , withAssetLoader
    , withInitState
    , withFinalizer
    , withLoaderResource
    , withLogger
    , withTargetRefreshRate
    , withStartup
    , withSystem
    , withTitle
    ) where

import           Verda.App.Internal