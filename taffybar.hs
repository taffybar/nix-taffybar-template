{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Default (def)
import Paths_nix_taffybar_template (getDataFileName)
import System.Taffybar (startTaffybar)
import System.Taffybar.Hooks (withLogServer, withToggleServer)
import System.Taffybar.Information.CPU (cpuLoad)
import System.Taffybar.Information.Memory (memoryUsedRatio, parseMeminfo)
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph (GraphConfig (..))
import System.Taffybar.Widget.Generic.PollingGraph (pollingGraphNew)

type RGBA = (Double, Double, Double, Double)

transparent, mint, cyan, mauve :: RGBA
transparent = (0.0, 0.0, 0.0, 0.0)
mint = (0.42, 0.86, 0.65, 0.95)
cyan = (0.42, 0.72, 0.95, 0.95)
mauve = (0.80, 0.52, 0.94, 0.65)

graphBaseConfig :: GraphConfig
graphBaseConfig =
  def
    { graphPadding = 0,
      graphBorderWidth = 0,
      graphWidth = 72,
      graphBackgroundColor = transparent
    }

cpuGraphConfig, memGraphConfig :: GraphConfig
cpuGraphConfig =
  graphBaseConfig
    { graphDataColors = [mint, mauve],
      graphLabel = Just "cpu"
    }
memGraphConfig =
  graphBaseConfig
    { graphDataColors = [cyan],
      graphLabel = Just "mem"
    }

memoryGraphValues :: IO [Double]
memoryGraphValues = do
  memoryInfo <- parseMeminfo
  pure [memoryUsedRatio memoryInfo]

cpuGraphValues :: IO [Double]
cpuGraphValues = do
  (_, systemLoad, totalLoad) <- cpuLoad
  pure [totalLoad, systemLoad]

main :: IO ()
main = do
  cssPath <- getDataFileName "taffybar.css"
  let simpleConfig =
        defaultSimpleTaffyConfig
          { startWidgets =
              map
                (>>= buildContentsBox)
                [ workspacesNew def,
                  layoutNew def,
                  windowsNew def
                ],
            centerWidgets =
              [ textClockNew Nothing "%a %b %_d  %H:%M" 5
                  >>= buildContentsBox
              ],
            endWidgets =
              map
                (>>= buildContentsBox)
                [ sniTrayNew,
                  batteryIconNew,
                  pollingGraphNew cpuGraphConfig 0.5 cpuGraphValues,
                  pollingGraphNew memGraphConfig 1.0 memoryGraphValues,
                  mpris2New
                ],
            barHeight = ExactSize 42,
            barPadding = 8,
            widgetSpacing = 0,
            cssPaths = [cssPath]
          }
  startTaffybar $ withLogServer $ withToggleServer $ toTaffybarConfig simpleConfig
