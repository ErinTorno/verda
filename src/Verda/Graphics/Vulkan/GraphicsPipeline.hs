{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}

module Verda.Graphics.Vulkan.GraphicsPipeline where

import           Control.Monad.Managed
import           Data.Bits
import qualified Data.Vector                  as Vec
import qualified Vulkan.Core10                as V
import           Vulkan.CStruct.Extends       (SomeStruct(..))
import qualified Vulkan.Zero                  as V

import           Verda.Graphics.Vulkan.Device   (VulkanDevice(..))
import           Verda.Graphics.Vulkan.Internal (allocate)
import           Verda.Graphics.Vulkan.Shader   (createShaders)
import           Verda.Util.Error               (sayErrAndExit)

createGraphicsPipeline :: VulkanDevice -> V.RenderPass -> Managed V.Pipeline
createGraphicsPipeline VulkanDevice{..} renderPass = do
    shaderStages   <- createShaders vdDevice
    pipelineLayout <- V.withPipelineLayout vdDevice V.zero Nothing allocate
    let pipelineCreateInfo :: V.GraphicsPipelineCreateInfo '[]
        pipelineCreateInfo = V.zero
            { V.stages             = shaderStages
            , V.viewportState      = Just . SomeStruct $ V.zero
                { V.viewports = [V.Viewport { V.minDepth = 0, V.maxDepth = 1, V.x = 0, V.y = 0
                                            , V.width    = realToFrac (V.width (vdExtent :: V.Extent2D))
                                            , V.height   = realToFrac (V.height (vdExtent :: V.Extent2D))
                                            }]
                , V.scissors  = [V.Rect2D {V.offset = V.Offset2D 0 0, V.extent = vdExtent}]
                }
            , V.rasterizationState = SomeStruct $ V.zero
                { V.depthClampEnable        = False
                , V.rasterizerDiscardEnable = False
                , V.lineWidth               = 1
                , V.polygonMode             = V.POLYGON_MODE_FILL
                , V.cullMode                = V.CULL_MODE_NONE
                , V.frontFace               = V.FRONT_FACE_CLOCKWISE
                , V.depthBiasEnable         = False
                }
            , V.multisampleState = Just . SomeStruct $ V.zero
                { V.sampleShadingEnable  = False
                , V.rasterizationSamples = V.SAMPLE_COUNT_1_BIT
                , V.minSampleShading     = 1
                , V.sampleMask           = [maxBound]
                }
            , V.colorBlendState = Just . SomeStruct $ V.zero
                { V.logicOpEnable = False
                , V.attachments   = [V.zero {V.blendEnable = False, V.colorWriteMask = V.COLOR_COMPONENT_R_BIT .|. V.COLOR_COMPONENT_G_BIT .|. V.COLOR_COMPONENT_B_BIT .|. V.COLOR_COMPONENT_A_BIT}]
                }
            , V.vertexInputState   = Just V.zero
            , V.inputAssemblyState = Just V.zero {V.topology = V.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, V.primitiveRestartEnable = False}
            , V.depthStencilState  = Nothing
            , V.dynamicState       = Nothing
            , V.layout             = pipelineLayout
            , V.renderPass         = renderPass
            , V.subpass            = 0
            , V.basePipelineHandle = V.zero
            }
    (snd <$> V.withGraphicsPipelines vdDevice V.zero [SomeStruct pipelineCreateInfo] Nothing allocate) >>= \case
        []        -> sayErrAndExit "No Vulkan Graphics Pipelines found"
        pipelines -> pure $ Vec.head pipelines

