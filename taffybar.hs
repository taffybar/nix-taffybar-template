{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Default (def)
import Paths_nix_taffybar_template (getDataFileName)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (Backend (..), detectBackend)
import System.Taffybar.Hooks (withLogServer, withToggleServer)
import System.Taffybar.Information.CPU2 (CPULoad (..), sampleCPULoad)
import System.Taffybar.Information.Memory (memoryUsedRatio, parseMeminfo)
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph (GraphConfig (..))
import System.Taffybar.Widget.Generic.PollingGraph (pollingGraphNew)
import System.Taffybar.Widget.HyprlandLayout (hyprlandLayoutNew)
import qualified System.Taffybar.Widget.Windows as Windows
import qualified System.Taffybar.Widget.Workspaces as Workspaces

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
  CPULoad {cpuSystemLoad = systemLoad, cpuTotalLoad = totalLoad} <- sampleCPULoad 0.05 "cpu"
  pure [totalLoad, systemLoad]

workspaceWidget =
  Workspaces.workspacesNew $
    Workspaces.defaultWorkspacesConfig
      { Workspaces.showWorkspaceFn = Workspaces.hideEmpty
      }

windowsWidget = Windows.windowsNew Windows.defaultWindowsConfig

main :: IO ()
main = do
  cssPath <- getDataFileName "taffybar.css"
  backendType <- detectBackend
  let layoutWidget =
        case backendType of
          BackendX11 -> layoutNew def
          BackendWayland -> hyprlandLayoutNew def
  let simpleConfig =
        defaultSimpleTaffyConfig
          { startWidgets =
              map
                (>>= buildContentsBox)
                [ workspaceWidget,
                  layoutWidget,
                  windowsWidget
                ],
            centerWidgets =
              [ textClockNew Nothing "%a %b %_d  %H:%M" 5
                  >>= buildContentsBox
              ],
            endWidgets =
              map
                (>>= buildContentsBox)
                [ sniTrayNew,
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
