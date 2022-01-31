{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}

module Verda.Graphics.Vulkan.Window where

import           Control.Exception            (bracket, bracket_)
import           Control.Monad
import           Control.Monad.Managed
import           Data.Bits
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Default
import           Data.IORef
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Vector                  as Vec
import           Foreign.Ptr
import           Linear.V2
import qualified SDL
import qualified SDL.Video.Vulkan             as SDL
import           Vulkan.CStruct.Extends       (pattern (:&), pattern (::&), SomeStruct(..))
import qualified Vulkan.Core10                as V
import qualified Vulkan.Extensions                            as V.KHR
import qualified Vulkan.Extensions.VK_EXT_debug_utils         as V.EXT
import qualified Vulkan.Extensions.VK_EXT_validation_features as V.EXT
import           Vulkan.Utils.Debug           (debugCallbackPtr)
import qualified Vulkan.Zero                  as V

import           Verda.Graphics.Components
import qualified Verda.Graphics.SDL                     as SDL
import           Verda.Graphics.Vulkan.Buffers          (createCommandBuffers, createFrameBuffers)
import           Verda.Graphics.Vulkan.Device           (createVulkanDevice)
import           Verda.Graphics.Vulkan.GraphicsPipeline (createGraphicsPipeline)
import           Verda.Graphics.Vulkan.Internal         (allocate)
import           Verda.Graphics.Vulkan.RenderPass       (createRenderPass)
import           Verda.Graphics.Vulkan.Types
import           Verda.Graphics.Vulkan.Vertex           (createVertexBuffer)
import           Verda.Util.Logger

maxFramesInFlight :: Num a => a
maxFramesInFlight = 2

run :: WindowCreateInfo -> (IORef VulkanWindow -> Managed () -> IO ()) -> Managed ()
run winCreateInfo loop = do
    withSDL
    window@VulkanWindow{..} <- withVulkanWindow winCreateInfo
    SDL.showWindow vwSDLWindow
    frameState <- liftIO $ newIORef 0
    windowIORef <- liftIO $ newIORef window
    liftIO $ loop windowIORef (drawFrame winCreateInfo windowIORef frameState)
    V.deviceWaitIdle (vdDevice vwDevice)

withSDL :: Managed ()
withSDL =
    let initSDL = do
            SDL.initializeAll
            SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
     in managed_ $ bracket_ initSDL SDL.quit 
                 . bracket_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary

withWindowSurface :: V.Instance -> SDL.Window -> Managed V.KHR.SurfaceKHR
withWindowSurface inst window = managed $ bracket
    (V.KHR.SurfaceKHR <$> SDL.vkCreateSurface window (castPtr (V.instanceHandle inst)))
    (\s -> V.KHR.destroySurfaceKHR inst s Nothing)

withSDLWindow :: Text -> SDL.WindowConfig -> Managed SDL.Window
withSDLWindow appTitle winConfig = managed $ bracket (SDL.createWindow appTitle winConfig) SDL.destroyWindow

withVulkanWindow :: WindowCreateInfo -> Managed VulkanWindow
withVulkanWindow WindowCreateInfo{..} = do
    let context = SDL.windowGraphicsContext sdlWindowConfig
        (V2 winW winH)  = fromIntegral <$> SDL.windowInitialSize sdlWindowConfig
        preferredFormat = V.KHR.SurfaceFormatKHR V.FORMAT_B8G8R8_UNORM V.KHR.COLOR_SPACE_SRGB_NONLINEAR_KHR
    when (context /= SDL.VulkanContext) $
        logLineWith logger Error $ "withVulkanWindow was called with a SDL.WindowConfig with a non-VulkanContext (was " <> T.pack (show context) <> ")"
    vwSDLWindow <- withSDLWindow appTitle sdlWindowConfig
    instInfo    <- sdlWindowInstanceCreateInfo appTitle logger vwSDLWindow
    inst        <- V.withInstance instInfo Nothing allocate

    void $ V.EXT.withDebugUtilsMessengerEXT inst debugUtilsMessengerCreateInfoExt Nothing allocate
    V.EXT.submitDebugUtilsMessageEXT inst
                                     V.EXT.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                                     V.EXT.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                                     V.zero

    vwSDLRenderer              <- SDL.createRenderer vwSDLWindow (-1) SDL.defaultRenderer
    surface                    <- withWindowSurface inst vwSDLWindow
    vwDevice                   <- createVulkanDevice inst surface preferredFormat winW winH
    vwRenderPass               <- createRenderPass vwDevice
    vwGraphicsPipeline         <- createGraphicsPipeline vwDevice vwRenderPass
    vwFrameBuffers             <- createFrameBuffers vwDevice vwRenderPass
    vwCommandBuffers           <- createCommandBuffers vwDevice vwRenderPass vwGraphicsPipeline vwFrameBuffers
    vwImageAvailableSemaphores <- Vec.fromList <$> replicateM maxFramesInFlight (V.withSemaphore (vdDevice vwDevice) V.zero Nothing allocate)
    vwRenderFinishedSemaphore  <- V.withSemaphore (vdDevice vwDevice) V.zero Nothing allocate
    vwVertexBuffer             <- createVertexBuffer vwDevice
    pure $ VulkanWindow{..}

sdlWindowInstanceCreateInfo :: MonadIO m => Text -> Logger -> SDL.Window -> m (V.InstanceCreateInfo '[V.EXT.DebugUtilsMessengerCreateInfoEXT, V.EXT.ValidationFeaturesEXT])
sdlWindowInstanceCreateInfo title logger window = liftIO $ do
    let mkSet f = Set.fromList . Vec.toList . fmap f . snd
    winExtens           <- traverse BS.packCString =<< SDL.vkGetInstanceExtensions window

    availableExtenNames <- mkSet V.extensionName <$> V.enumerateInstanceExtensionProperties Nothing
    availableLayerNames <- mkSet V.layerName     <$> V.enumerateInstanceLayerProperties

    let requiredExtens = [V.EXT.EXT_DEBUG_UTILS_EXTENSION_NAME] <> Set.fromList winExtens
        optionalExtens = [V.EXT.EXT_VALIDATION_FEATURES_EXTENSION_NAME]
        requiredLayers = []
        optionalLayers = ["VK_LAYER_KHRONOS_validation"]

    validExtens <- filterAvailableExt logger "extension" availableExtenNames requiredExtens optionalExtens
    validLayers <- filterAvailableExt logger "layer"     availableLayerNames requiredLayers optionalLayers
    
    pure $ V.zero
        { V.applicationInfo = Just V.zero { V.applicationName = Just (T.encodeUtf8 title)
                                          , V.apiVersion      = V.API_VERSION_1_0 }
        , V.enabledExtensionNames = Vec.fromList $ Set.toList validExtens
        , V.enabledLayerNames     = Vec.fromList $ Set.toList validLayers
        }
        ::& debugUtilsMessengerCreateInfoExt
        :&  V.EXT.ValidationFeaturesEXT [V.EXT.VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []
        :&  ()

filterAvailableExt :: MonadIO m => Logger -> Text -> Set ByteString -> Set ByteString -> Set ByteString -> m (Set ByteString)
filterAvailableExt logger label avail req opt = do
    let (reqPres, reqMis) = Set.partition (`Set.member` avail) req
        (optPres, optMis) = Set.partition (`Set.member` avail) opt
    unless (Set.null reqMis) $
        logLineWith logger Error   $ "Missing required " <> label <> T.pack (show reqMis)
    unless (Set.null optMis) $
        logLineWith logger Warning $ "Missing optional " <> label <> T.pack (show optMis)
    pure $ reqPres <> optPres

drawFrame :: WindowCreateInfo -> IORef VulkanWindow -> IORef Int -> Managed ()
drawFrame winCreateInfo windowIORef ioref = do
    VulkanWindow{..} <- liftIO $ readIORef windowIORef
    iaSemaIdx        <- liftIO $ readIORef ioref
    let VulkanDevice{..}    = vwDevice
        imageAvailSemaphore = vwImageAvailableSemaphores Vec.! iaSemaIdx
    (result, imgIdx) <- V.KHR.acquireNextImageKHR vdDevice vdSwapChain maxBound imageAvailSemaphore V.zero
    if result == V.ERROR_OUT_OF_DATE_KHR || result == V.ERROR_OUT_OF_DATE_KHR 
    then do
        liftIO $ putStrLn "remaking window and continuing"
        window' <- withVulkanWindow winCreateInfo
        liftIO $ writeIORef windowIORef window'
    else do
        let submitInfo = V.zero
                { V.waitSemaphores   = [imageAvailSemaphore]
                , V.waitDstStageMask = [V.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                , V.commandBuffers   = [V.commandBufferHandle $ vwCommandBuffers Vec.! fromIntegral imgIdx]
                , V.signalSemaphores = [vwRenderFinishedSemaphore]
                }
            presentInfo = V.zero
                { V.KHR.waitSemaphores = [vwRenderFinishedSemaphore]
                , V.KHR.swapchains     = [vdSwapChain]
                , V.KHR.imageIndices   = [imgIdx]
                }
        V.queueSubmit vdGraphicsQueue [SomeStruct submitInfo] V.zero 
        void $ V.KHR.queuePresentKHR vdPresentQueue presentInfo
        V.queueWaitIdle vdPresentQueue
        liftIO $ writeIORef ioref ((iaSemaIdx + 1) `mod` maxFramesInFlight)

getDisplayModeRefreshRate :: VulkanWindow -> Logger -> IO Float
getDisplayModeRefreshRate VulkanWindow{..} logger =
    SDL.getDisplayModeRefreshRate logger (unTargetRefreshRate def) vwSDLWindow

------------
-- Config --
------------

debugUtilsMessengerCreateInfoExt :: V.EXT.DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfoExt = V.zero
    { V.EXT.pfnUserCallback = debugCallbackPtr
    , V.EXT.messageSeverity = V.EXT.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                          .|. V.EXT.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
    , V.EXT.messageType     = V.EXT.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                          .|. V.EXT.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                          .|. V.EXT.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
    }