module Verda.Asset.Types where

import           Apecs                   hiding (Map, Set)
import           Control.Concurrent.MVar
import           Control.Monad.Catch     (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.ST        (RealWorld)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.Default
import qualified Dhall
import           Data.Dynamic            (Dynamic)
import           Data.Hashable
import           Data.HashTable.ST.Basic (HashTable)
import           Data.Map.Strict         (Map)
import           Data.Sequence           (Seq)
import           Data.Set                (Set)
import           Data.String             (IsString)
import           Data.Text               (Text)
import           Data.Vector             (Vector)
import           Data.Vector.Mutable     (IOVector)
import           GHC.Generics
import qualified System.FSNotify         as FSNotify
import           Type.Reflection

newtype Path = Path {unPath :: FilePath} deriving (Eq, Generic, Hashable, IsString, Ord, Read, Show)
instance Dhall.FromDhall Path where
    autoWith _ = Dhall.genericAutoWith (Dhall.defaultInterpretOptions {Dhall.singletonConstructors = Dhall.Bare})

newtype Handle a = Handle {unHandle :: Int} deriving (Eq, Generic, Hashable, Ord, Read, Show)

data AssetStatus
    = Failed String
    -- ^ The asset could not be loaded
    | NotLoaded
    -- ^ The asset is awaiting loading
    | Loading
    -- ^ The asset is being loaded
    | WaitingOnDependencies
    -- ^ The asset is loaded, but some of its dependencies might still be loading
    | Loaded
    -- ^ The asset and all its dependencies are loaded
    deriving (Eq, Generic, Ord, Read, Show)
instance Dhall.FromDhall AssetStatus

mostSevere :: AssetStatus -> AssetStatus -> AssetStatus
mostSevere a b = if a <= b then a else b

isFinished :: AssetStatus -> Bool
isFinished Loaded     = True
isFinished (Failed _) = True
isFinished _          = False

data AssetSettings = AssetSettings
    { assetFolder     :: !FilePath
    -- ^ What to use as the root directory for these assets (default "assets")
    , defaultAssetLen :: !Int
    -- ^ How many slots we initially allocate for asset handles (default 64)
    , threadDelayMS   :: !Int
    -- ^ How long to wait after processing before we should try again (default 15)
    , readAssetFile   :: FilePath -> IO ByteString
    -- ^ How the asset files' bytes are loaded (default Data.ByteString.readFile)
    , useHotReloading :: !Bool
    } deriving (Generic)

instance Default AssetSettings where
    def = AssetSettings
        { assetFolder     = "assets"
        , defaultAssetLen = 64
        , threadDelayMS   = 15
        , readAssetFile   = BS.readFile
        , useHotReloading = False
        }

data Loader = Loader
    { loaderTypeRef    :: !SomeTypeRep
    , isSingleThreaded :: !Bool
    , loadFn           :: !(AssetLoadFn Dynamic)
    }

data Assets = Assets
    { assetSettings            :: !AssetSettings
    -- ^ The settings to use when loading assets
    , assetLoaders             :: !(Map Text Loader)
    -- ^ A map of extensions to functions that can load that type of asset
    , loaderResources          :: !(Map SomeTypeRep Dynamic)
    -- ^ A map of SomeTypeReps to values that can be accessed by AssetLoaders
    , nextHandleID             :: !(MVar Int)
    -- ^ A counter of unique Handle IDs, starting at 0
    , loadedAssets             :: !(MVar (IOVector (Maybe Dynamic)))
    -- ^ A vector of assets, either empty as Nothing, or loaded as Just
    , assetStatuses            :: !(MVar (IOVector AssetStatus))
    -- ^ A vector of statuses for assets with the given Handle index
    , handlesByPath            :: !(MVar (HashTable RealWorld Path (Handle ())))
    -- ^ A hashtable of asset paths to associated handles
    , assetsToLoad             :: !(MVar (Seq (Int, AssetLoadFn Dynamic, AssetInfo ())))
    -- ^ A sequence of tuples of (Handle index, loader function, asset info) that are awaiting loading
    , assetsToLoadSingThreaded :: !(MVar (Seq (Int, AssetLoadFn Dynamic, AssetInfo ())))
    -- ^ A sequence of tuples of (Handle index, loader function, asset info) that are awaiting loading in a single-threaded environment only
    , assetsWaiting            :: !(MVar (Seq (Int, Either (Handle ()) (LoadedAsset Dynamic))))
    -- ^ A sequence of tuples of (Handle index, loaded asset) that are loaded themselves, but might not have all their dependencies loaded
    , assetsWaitingHotReloadWatching :: !(MVar (Seq HotReloadRequest))
    , assetStopWatchingsByPath       :: !(MVar (HashTable RealWorld Path FSNotify.StopListening))
    }

instance Semigroup Assets where (<>) = mappend
instance Monoid    Assets where mempty = error "Assets was not initialized before being used; This is a bug and should never happen"
instance Component Assets where type Storage Assets = Global Assets

newtype LoadContext a = LoadContext {unContext :: ReaderT Assets IO a} deriving (Applicative, Functor, Monad, MonadCatch, MonadIO, MonadMask, MonadReader Assets, MonadThrow)

data LabeledAsset = LabeledAsset
    { label        :: !Text
    -- ^ The name that identifies this sub-asset
    , labeledAsset :: !Dynamic
    -- ^ The asset in the form of a Dynamic
    }

newtype Dependency = Depedency {unDependency :: Handle ()} deriving (Eq, Generic, Hashable, Read, Show)

data LoadResult a = LoadFailure String | LoadSuccess (LoadedAsset a) | HandleAlias (Handle a) deriving (Eq, Generic, Read, Show)

data LoadedAsset a = LoadedAsset
    { asset         :: !a
    -- ^ The loaded asset
    , dependencies  :: !(Vector Dependency)
    -- ^ All dependent assets this asset and the assets required to be loaded before the asset itself can be considered loaded
    } deriving (Eq, Functor, Generic, Read, Show)

data AssetInfo a = AssetInfo
    { assetPath :: !Path
    , assetHandle :: Handle a
    } deriving (Eq, Generic, Ord, Read, Show)

type AssetLoadFn a = AssetInfo a -> ByteString -> LoadContext (LoadResult a)

class a `CanLoad` r where
    -- | True if asset can only be loaded in a singlethreaded environment
    isSingleThreadOnly :: Proxy (a r) -> Bool
    isSingleThreadOnly _ = False
    extensions :: Proxy (a r) -> Set Text
    loadAsset  :: Proxy (a r) -> AssetLoadFn r

newtype LoadSetElement = LoadSetElement {unLoadSetElement :: Handle ()} deriving (Eq, Generic, Ord, Read, Show)

newtype AssetLoadSet = AssetLoadSet {unAssetLoadSet :: [LoadSetElement]} deriving (Eq, Generic, Ord, Read, Show)

instance Default AssetLoadSet where
    def = AssetLoadSet []

data HotReloadRequest = HotReloadRequest
    { hrrPath          :: !Path
    , hrrIsMultiThread :: !Bool
    }