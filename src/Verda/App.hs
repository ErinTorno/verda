module Verda.App
    ( module Verda.App.System
    , App(..)
    , makeApp
    , makeAppWith
    , start
    , updateWindowConfig
    , withAssetLoader
    , withDefaultSystems
    , withIcon
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
import           Verda.App.System