{-# LANGUAGE DuplicateRecordFields #-}

module Verda.Graphics.Vulkan.Vertex where

import           Control.Monad.Managed
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vec
import qualified Vulkan.Core10                as V
import qualified Vulkan.Zero                  as V

import           Verda.Graphics.Vulkan.Internal (allocate)
import           Verda.Graphics.Vulkan.Types

maxVertexCount :: Num a => a
maxVertexCount = 10000

bindingDescription :: V.VertexInputBindingDescription
bindingDescription = V.VertexInputBindingDescription
    { V.binding = 0
    , V.stride  = 4 * 5
    , V.inputRate = V.VERTEX_INPUT_RATE_VERTEX -- VERTEX_INPUT_RATE_VERTEX todo?
    }

attributeDescriptions :: Vector V.VertexInputAttributeDescription
attributeDescriptions = Vec.fromList
    [ V.zero {V.binding = 0, V.location = 0, V.offset = 0,     V.format = V.FORMAT_R32G32_SFLOAT}
    , V.zero {V.binding = 0, V.location = 1, V.offset = 4 * 3, V.format = V.FORMAT_R32G32B32_SFLOAT}
    ]

createVertexBuffer :: VulkanDevice -> Managed V.Buffer
createVertexBuffer VulkanDevice{..} =
    let info = V.zero { V.size        = 4 * 5 * maxVertexCount
                      , V.usage       = V.BUFFER_USAGE_VERTEX_BUFFER_BIT
                      , V.sharingMode = V.SHARING_MODE_EXCLUSIVE}
     in V.withBuffer vdDevice info Nothing allocate