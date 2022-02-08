module Verda.Graphics.Vulkan.Swapchain where
    
import           Data.Either
import           UnliftIO
import qualified Vulkan.Core10                as V
import qualified Vulkan.Exception             as V

shouldRemakeSwapchain :: MonadUnliftIO m => m a -> m Bool
shouldRemakeSwapchain action =
    let isRemake = \case
            V.VulkanException V.ERROR_OUT_OF_DATE_KHR -> Just V.ERROR_OUT_OF_DATE_KHR 
            _ -> Nothing
     in fmap isLeft . tryJust isRemake $ action