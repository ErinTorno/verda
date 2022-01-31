{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}

module Verda.Graphics.Vulkan.Buffers
    ( createCommandBuffers
    , createFrameBuffers
    ) where

import           Control.Monad.Managed
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vec
import qualified Vulkan.Core10                as V
import qualified Vulkan.Zero                  as V

import           Verda.Graphics.Vulkan.Internal (allocate)
import           Verda.Graphics.Vulkan.Types

createCommandBuffers :: VulkanDevice -> V.RenderPass -> V.Pipeline -> Vector V.Framebuffer -> Managed (Vector V.CommandBuffer)
createCommandBuffers VulkanDevice{..} !renderPass !graphicsPipeline !frameBuffers = do
    commandPool <- V.withCommandPool vdDevice (V.zero {V.queueFamilyIndex = vdGraphicsQueueFamilyIdx}) Nothing allocate
    let bufferAllocateInfo = V.zero
            { V.commandPool = commandPool
            , V.level       = V.COMMAND_BUFFER_LEVEL_PRIMARY
            , V.commandBufferCount = fromIntegral $ Vec.length frameBuffers
            }
    buffers <- V.withCommandBuffers vdDevice bufferAllocateInfo allocate
    liftIO . Vec.forM_ (Vec.zip frameBuffers buffers) $ \(frameBuffer, buffer) ->
        V.useCommandBuffer buffer V.zero {V.flags = V.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT} $
            let renderPassBeginInfo = V.zero
                    { V.renderPass  = renderPass
                    , V.framebuffer = frameBuffer
                    , V.renderArea  = V.Rect2D {V.offset = V.zero, V.extent = vdExtent}
                    , V.clearValues = [V.Color (V.Float32 0.1 0.1 0.1 0)]
                    }
             in V.cmdUseRenderPass buffer renderPassBeginInfo V.SUBPASS_CONTENTS_INLINE $ do
                    V.cmdBindPipeline buffer V.PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
                    V.cmdDraw buffer 3 1 0 0
    pure buffers

createFrameBuffers :: VulkanDevice -> V.RenderPass -> Managed (Vector V.Framebuffer)
createFrameBuffers VulkanDevice{..} !renderPass =
    Vec.forM vdSwapChainImageViews $ \imageView ->
        let createInfo = V.zero
                { V.renderPass  = renderPass
                , V.attachments = [imageView]
                , V.width       = V.width (vdExtent :: V.Extent2D)
                , V.height      = V.height (vdExtent :: V.Extent2D)
                , V.layers      = 1
                }
         in V.withFramebuffer vdDevice createInfo Nothing allocate