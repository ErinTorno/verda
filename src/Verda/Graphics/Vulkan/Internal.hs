module Verda.Graphics.Vulkan.Internal where

import           Control.Exception            (bracket)
import           Control.Monad.Managed

allocate :: IO a -> (a -> IO ()) -> Managed a
allocate mk action = managed (bracket mk action)