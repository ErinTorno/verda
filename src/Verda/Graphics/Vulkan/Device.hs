{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}

module Verda.Graphics.Vulkan.Device where

import           Control.Monad.Managed
import           Data.Bits
import           Data.Default
import           Data.Function                (on)
import           Data.Functor                 ((<&>))
import           Data.Ord                     (comparing)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vec
import           Data.Word
import           Vulkan.CStruct.Extends       (SomeStruct(..))
import qualified Vulkan.Core10                as V
import qualified Vulkan.Extensions.VK_KHR_surface             as V.KHR
import qualified Vulkan.Extensions.VK_KHR_swapchain           as V.KHR
import qualified Vulkan.Zero                  as V

import           Verda.Graphics.Vulkan.Internal (allocate)
import           Verda.Graphics.Vulkan.Types
import           Verda.Util.Logger

getFormat :: MonadIO m => V.PhysicalDevice -> V.KHR.SurfaceKHR -> V.KHR.SurfaceFormatKHR -> m V.KHR.SurfaceFormatKHR
getFormat !device !surface !preferred = fmap snd (V.KHR.getPhysicalDeviceSurfaceFormatsKHR device surface) <&> \case
        [] -> preferred
        [V.KHR.SurfaceFormatKHR V.FORMAT_UNDEFINED _] -> preferred
        formats -> if   Vec.any (\f -> ((==) `on` V.KHR.format) f preferred && ((==) `on` V.KHR.colorSpace) f preferred) formats
                   then preferred
                   else Vec.head formats

getPresentMode :: MonadIO m => V.PhysicalDevice -> V.KHR.SurfaceKHR -> m (Maybe V.KHR.PresentModeKHR)
getPresentMode !device !surface = do
    let desiredModes = [V.KHR.PRESENT_MODE_MAILBOX_KHR, V.KHR.PRESENT_MODE_FIFO_KHR, V.KHR.PRESENT_MODE_IMMEDIATE_KHR]
        maybeHead m  = if   Vec.null m
                       then pure Nothing
                       else Just <$> Vec.headM m
    modes <- snd <$> V.KHR.getPhysicalDeviceSurfacePresentModesKHR device surface
    maybeHead . Vec.filter (`Vec.elem` modes) $ desiredModes

getGraphicsQueueIndices :: MonadIO m => V.PhysicalDevice -> m (Vector Word32)
getGraphicsQueueIndices !device = do
    queueProps <- V.getPhysicalDeviceQueueFamilyProperties device
    pure . fmap (fromIntegral . fst)
         . Vec.filter (\(_, prop) -> (V.QUEUE_GRAPHICS_BIT .&. V.queueFlags prop) /= zeroBits && (V.queueCount prop > 0))
         $ Vec.indexed queueProps

getPresentQueueIndices :: MonadIO m => V.PhysicalDevice -> V.KHR.SurfaceKHR -> m (Vector Word32)
getPresentQueueIndices !device !surface = do
    queueProps <- V.getPhysicalDeviceQueueFamilyProperties device
    Vec.filterM
        (\i -> V.KHR.getPhysicalDeviceSurfaceSupportKHR device i surface)
        (Vec.generate (Vec.length queueProps) fromIntegral)

getTotalSize :: MonadIO m => V.PhysicalDevice -> m Word64
getTotalSize !device = sumMemory <$> V.getPhysicalDeviceMemoryProperties device
    where sumMemory = sum
                    . fmap (V.size :: V.MemoryHeap -> V.DeviceSize)
                    . V.memoryHeaps

hasSwapChain :: MonadIO m => V.PhysicalDevice -> m Bool
hasSwapChain !device = Vec.any ((== V.KHR.KHR_SWAPCHAIN_EXTENSION_NAME) . V.extensionName) . snd
                   <$> V.enumerateDeviceExtensionProperties device Nothing

createVulkanDevice :: V.Instance -> V.KHR.SurfaceKHR -> V.KHR.SurfaceFormatKHR -> Word32 -> Word32 -> Managed VulkanDevice
createVulkanDevice !inst !surface !preferredFormat !winW !winH =
    getBestPhysicalDevice inst surface >>= \case
        Nothing ->
            logAndExitWith def Error "No suitable PhysicalDevice found"
        Just (vdTotalSize, (vdPhysicalDevice, vdGraphicsQueueFamilyIdx, vdPresentQueueFamilyIdx, vdPresentMode)) -> do
            let uniqueFamilyIndices = if vdGraphicsQueueFamilyIdx == vdPresentQueueFamilyIdx
                                    then [vdGraphicsQueueFamilyIdx]
                                    else [vdGraphicsQueueFamilyIdx, vdPresentQueueFamilyIdx]
                deviceCreateInfo = V.zero
                    { V.queueCreateInfos = (\idx -> SomeStruct $ V.zero {V.queueFamilyIndex = idx, V.queuePriorities = [1]}) <$> uniqueFamilyIndices
                    , V.enabledExtensionNames = [V.KHR.KHR_SWAPCHAIN_EXTENSION_NAME]
                    }
                vdSurface = surface
            vdSurfaceFormat         <- getFormat vdPhysicalDevice surface preferredFormat
            vdSurfaceCapabilities   <- V.KHR.getPhysicalDeviceSurfaceCapabilitiesKHR vdPhysicalDevice surface
            vdDevice                <- V.withDevice vdPhysicalDevice deviceCreateInfo Nothing allocate
            vdGraphicsQueue         <- V.getDeviceQueue vdDevice vdGraphicsQueueFamilyIdx 0
            vdPresentQueue          <- V.getDeviceQueue vdDevice vdPresentQueueFamilyIdx 0
            (vdSwapChain, vdExtent) <- createSwapChain surface vdDevice vdSurfaceFormat vdSurfaceCapabilities vdPresentMode vdGraphicsQueue vdPresentQueue vdGraphicsQueueFamilyIdx vdPresentQueueFamilyIdx winW winH
            vdSwapChainImageViews   <- createSwapChainImageViews vdDevice vdSwapChain $ V.KHR.format vdSurfaceFormat
            let vdFormat = V.KHR.format vdSurfaceFormat
            pure $ VulkanDevice{..}

createSwapChain :: V.KHR.SurfaceKHR
                -> V.Device
                -> V.KHR.SurfaceFormatKHR
                -> V.KHR.SurfaceCapabilitiesKHR
                -> V.KHR.PresentModeKHR
                -> V.Queue
                -> V.Queue
                -> Word32
                -> Word32
                -> Word32
                -> Word32
                -> Managed (V.KHR.SwapchainKHR, V.Extent2D)
createSwapChain !surface !device !surfaceFormat !capabilities !presentMode !graphicsQueue !presentQueue !graphicsQueueIdx !presentQueueIdx !winWidth !winHeight = (,)
    <$> V.KHR.withSwapchainKHR device createInfo Nothing allocate
    <*> pure (V.KHR.imageExtent (createInfo :: V.KHR.SwapchainCreateInfoKHR '[]))
    where (sharingMode, queueFamilyIndices) = if graphicsQueue == presentQueue
                                              then (V.SHARING_MODE_EXCLUSIVE, [])
                                              else (V.SHARING_MODE_CONCURRENT, [graphicsQueueIdx, presentQueueIdx])
          extent = case V.KHR.currentExtent capabilities of
              V.Extent2D w h | w == maxBound, h == maxBound -> V.Extent2D winHeight winWidth
              e -> e
          createInfo = V.zero
              { V.KHR.surface            = surface
              , V.KHR.minImageCount      = V.KHR.minImageCount (capabilities :: V.KHR.SurfaceCapabilitiesKHR) + 1
              , V.KHR.imageArrayLayers   = 1
              , V.KHR.imageColorSpace    = V.KHR.colorSpace surfaceFormat
              , V.KHR.imageExtent        = extent
              , V.KHR.imageFormat        = V.KHR.format surfaceFormat
              , V.KHR.imageSharingMode   = sharingMode
              , V.KHR.imageUsage         = V.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
              , V.KHR.queueFamilyIndices = queueFamilyIndices
              , V.KHR.preTransform       = V.KHR.currentTransform capabilities
              , V.KHR.compositeAlpha     = V.KHR.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
              , V.KHR.presentMode        = presentMode
              , V.KHR.clipped            = True
              }

createSwapChainImageViews :: V.Device -> V.KHR.SwapchainKHR -> V.Format -> Managed (Vector V.ImageView)
createSwapChainImageViews device swapChain format =                           
    let createInfo i = V.zero
            { V.image            = i
            , V.viewType         = V.IMAGE_VIEW_TYPE_2D
            , V.format           = format
            , V.components       = V.zero {V.r = V.COMPONENT_SWIZZLE_IDENTITY, V.g = V.COMPONENT_SWIZZLE_IDENTITY, V.b = V.COMPONENT_SWIZZLE_IDENTITY, V.a = V.COMPONENT_SWIZZLE_IDENTITY}
            , V.subresourceRange = V.zero {V.aspectMask = V.IMAGE_ASPECT_COLOR_BIT, V.baseMipLevel = 0, V.levelCount = 1, V.baseArrayLayer = 0, V.layerCount = 1}
            }
     in V.KHR.getSwapchainImagesKHR device swapChain
    >>= mapM (\i -> V.withImageView device (createInfo i) Nothing allocate) . snd

getBestPhysicalDevice :: V.Instance -> V.KHR.SurfaceKHR -> Managed (Maybe (Word64, (V.PhysicalDevice, Word32, Word32, V.KHR.PresentModeKHR)))
getBestPhysicalDevice !inst !surface = do
    devices <- snd <$> V.enumeratePhysicalDevices inst
    vulkanDevices <- fmap (Vec.mapMaybe id)
                   . Vec.forM devices $ \device -> do
        hasChain <- hasSwapChain device
        if not hasChain
        then pure Nothing
        else do
            ((,,) <$> getGraphicsQueueIndices device <*> getPresentQueueIndices device surface <*> getPresentMode device surface) >>= \case
                ([], _, _) -> pure Nothing
                (_, [], _) -> pure Nothing
                (_, _, Nothing) -> pure Nothing
                (graphicsQueue, presentQueue, Just mode) -> do
                    totalSize    <- getTotalSize device
                    pure $ Just (totalSize, (device, Vec.head graphicsQueue, Vec.head presentQueue, mode))
    if Vec.null vulkanDevices
    then pure Nothing
    else pure $ Just $ Vec.maximumBy (comparing fst) vulkanDevices