{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE QuasiQuotes           #-}

module Verda.Graphics.Vulkan.Shader
  ( createShaders
  ) where

import           Control.Monad.Managed
import           Data.ByteString                    (ByteString)
import           Data.Vector                        (Vector)
import qualified Vulkan.Core10                      as V
import qualified Vulkan.CStruct.Extends             as V
import           Vulkan.Utils.ShaderQQ.GLSL.Glslang (vert, frag)
import qualified Vulkan.Zero                        as V

import           Verda.Graphics.Vulkan.Internal     (allocate)

createShaders :: V.Device -> Managed (Vector (V.SomeStruct V.PipelineShaderStageCreateInfo))
createShaders device = do
    fragMod <- V.withShaderModule device V.zero { V.code = fragCode} Nothing allocate
    vertMod <- V.withShaderModule device V.zero { V.code = vertCode} Nothing allocate
    pure [ V.SomeStruct V.zero {V.stage   = V.SHADER_STAGE_VERTEX_BIT,   V.module' = vertMod, V.name = "main"}
         , V.SomeStruct V.zero {V.stage   = V.SHADER_STAGE_FRAGMENT_BIT, V.module' = fragMod, V.name = "main"}
         ]

fragCode :: ByteString
fragCode = [frag|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) in vec3 fragColor;

        layout(location = 0) out vec4 outColor;

        void main() {
            outColor = vec4(fragColor, 1.0);
        }
    |]

vertCode :: ByteString
vertCode = [vert|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) out vec3 fragColor;

        vec2 positions[3] = vec2[](
          vec2(0.0, -0.5),
          vec2(0.5, 0.5),
          vec2(-0.5, 0.5)
        );

        vec3 colors[3] = vec3[](
          vec3(1.0, 1.0, 0.0),
          vec3(0.0, 1.0, 1.0),
          vec3(1.0, 0.0, 1.0)
        );

        void main() {
          gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
          fragColor = colors[gl_VertexIndex];
        }
    |]