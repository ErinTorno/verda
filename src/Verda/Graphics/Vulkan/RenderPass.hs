{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}

module Verda.Graphics.Vulkan.RenderPass
    ( createRenderPass
    ) where

import           Control.Monad.Managed
import           Data.Bits
import qualified Vulkan.Core10                  as V
import qualified Vulkan.Zero                    as V

import           Verda.Graphics.Vulkan.Internal (allocate)
import           Verda.Graphics.Vulkan.Types

createRenderPass :: VulkanDevice -> Managed V.RenderPass
createRenderPass VulkanDevice{..} = V.withRenderPass vdDevice (V.zero {V.attachments = [attachmentDesc], V.subpasses = [subpass], V.dependencies = [subpassDep]}) Nothing allocate
    where attachmentDesc = V.zero
              { V.format         = vdFormat
              , V.samples        = V.SAMPLE_COUNT_1_BIT
              , V.loadOp         = V.ATTACHMENT_LOAD_OP_CLEAR
              , V.storeOp        = V.ATTACHMENT_STORE_OP_STORE
              , V.stencilLoadOp  = V.ATTACHMENT_LOAD_OP_DONT_CARE
              , V.stencilStoreOp = V.ATTACHMENT_STORE_OP_DONT_CARE
              , V.initialLayout  = V.IMAGE_LAYOUT_UNDEFINED
              , V.finalLayout    = V.IMAGE_LAYOUT_PRESENT_SRC_KHR
              }
          subpass = V.zero
              { V.pipelineBindPoint = V.PIPELINE_BIND_POINT_GRAPHICS
              , V.colorAttachments  = [V.zero {V.attachment = 0, V.layout = V.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL}]
              }
          subpassDep = V.zero
              { V.srcSubpass    = V.SUBPASS_EXTERNAL
              , V.dstSubpass    = 0
              , V.srcStageMask  = V.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
              , V.srcAccessMask = V.zero
              , V.dstStageMask  = V.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
              , V.dstAccessMask = V.ACCESS_COLOR_ATTACHMENT_READ_BIT .|. V.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
              }
