let Asset        = ./asset.dhall
let Path         = Asset.Path
let ScaleQuality = < Nearest | Linear | Anistropic >
let Config       = { file : Path, scaleQuality : ScaleQuality }
 in { Path          = Path
    , ScaleQuality  = ScaleQuality
    , Config        = Config
    , defaultConfig = { scaleQuality = ScaleQuality.Linear }
    }