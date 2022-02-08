
module Verda.Graphics.Vulkan.Types where

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Text                    (Text)
import           Data.Vector                  (Vector)
import           Data.Word
import qualified SDL
import           UnliftIO
import qualified Vulkan.Core10                as V
import qualified Vulkan.Extensions            as V.KHR

import           Verda.Util.Logger

data WindowCreateInfo = WindowCreateInfo
    { appTitle        :: !Text
    , sdlWindowConfig :: !SDL.WindowConfig
    , logger          :: !Logger
    }

data VulkanDevice = VulkanDevice
    { vdPhysicalDevice         :: !V.PhysicalDevice
    , vdDevice                 :: !V.Device
    , vdSurface                :: !V.KHR.SurfaceKHR
    , vdTotalSize              :: !Word64
    , vdGraphicsQueue          :: !V.Queue
    , vdGraphicsQueueFamilyIdx :: !Word32
    , vdPresentQueue           :: !V.Queue
    , vdPresentQueueFamilyIdx  :: !Word32
    , vdFormat                 :: !V.Format
    , vdSurfaceFormat          :: !V.KHR.SurfaceFormatKHR
    , vdPresentMode            :: !V.KHR.PresentModeKHR
    , vdSurfaceCapabilities    :: !V.KHR.SurfaceCapabilitiesKHR
    , vdSwapChain              :: !V.KHR.SwapchainKHR
    , vdSwapChainImageViews    :: !(Vector V.ImageView)
    , vdExtent                 :: !V.Extent2D
    }

data VulkanWindow = VulkanWindow
    { vwSDLWindow                :: !SDL.Window
    , vwSDLRenderer              :: !SDL.Renderer
    , vwDevice                   :: !VulkanDevice
    , vwRenderPass               :: !V.RenderPass
    , vwGraphicsPipeline         :: !V.Pipeline
    , vwFrameBuffers             :: !(Vector V.Framebuffer)
    , vwCommandBuffers           :: !(Vector V.CommandBuffer)
    , vwImageAvailableSemaphores :: !(Vector V.Semaphore)
    , vwRenderFinishedSemaphore  :: !V.Semaphore
    , vwVertexBuffer             :: !V.Buffer
    }

newtype VulkanT a = VulkanT {unVulkanT :: ReaderT VulkanWindow (ResourceT IO) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadResource)

instance MonadUnliftIO VulkanT where
    withRunInIO action = VulkanT $ withRunInIO $ \r ->
        action (r . unVulkanT)
