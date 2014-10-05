-- IMPORTS {{{
import XMonad

import System.IO

import qualified Data.Map as M

import Data.Ratio ((%))

import Graphics.X11.ExtraTypes.XF86

import XMonad.Util.Run
import XMonad.Util.Dzen
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.XSelection
import XMonad.Actions.Volume

import qualified XMonad.Prompt as P
import XMonad.Prompt.Window
import XMonad.Prompt.Shell

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders
import XMonad.Layout.BinarySpacePartition

import XMonad.StackSet (RationalRect(..))
-- end of IMPORTS }}}



-- MAIN {{{
main :: IO ()
main = do
    dzenHandle <- spawnPipe myStatusBar
    browser <- getBrowser
    xmonad $ ewmh $ withUrgencyHook NoUrgencyHook
           $ defaultConfig
      { modMask             = mod4Mask
      , terminal            = "urxvt"
      , workspaces          = map show [1..9]
      , keys                = \c -> myKeys c `M.union` keys defaultConfig c
      , layoutHook          = myLayouts
      , manageHook          = myManageHook <+> manageDocks
      , startupHook         = myStartupHook browser
      , logHook             = myLogHook dzenHandle
      , normalBorderColor   = colorBlueLight
      , focusedBorderColor  = colorFG
      , borderWidth         = 1
      , focusFollowsMouse   = True
      , handleEventHook     = handleEventHook defaultConfig <+>
                                fullscreenEventHook
      }
-- end of MAIN }}}


-- KEYS {{{
myKeys :: XConfig l -> M.Map (ButtonMask, KeySym) (X ())
myKeys XConfig{XMonad.modMask = modm} = M.fromList
      [ ((modm,               xK_asciicircum), spawn "slock")
      , ((modm .|. shiftMask, xK_asciicircum), do spawn "slock"
                                                  spawn "systemctl suspend")
      , ((modm,               xK_Return     ), spawn "urxvt")
      , ((modm,               xK_p          ), spawn "dmenu_run -b")

      , ((modm,               xK_Up         ), spawn "mpc -q next")
      , ((modm,               xK_Down       ), spawn "mpc -q prev")
      , ((modm,               xK_c          ), spawn "mpc -q toggle")
      , ((modm .|. shiftMask, xK_m          ), spawn "/home/koomi/bin/mpdmenu")
      , ((modm .|. shiftMask, xK_p          ), spawn "passmenu -b")

      , ((modm,               xK_u          ), focusUrgent)

      , ((0,         xF86XK_AudioRaiseVolume), raiseVolumeChannels ["Master"] 2 >>= alert)
      , ((0,         xF86XK_AudioLowerVolume), lowerVolumeChannels ["Master"] 2 >>= alert)

      , ((modm .|. altMask,               xK_l     ), sendMessage $ ExpandTowards R)
      , ((modm .|. altMask,               xK_h     ), sendMessage $ ExpandTowards L)
      , ((modm .|. altMask,               xK_j     ), sendMessage $ ExpandTowards D)
      , ((modm .|. altMask,               xK_k     ), sendMessage $ ExpandTowards U)
      , ((modm .|. altMask .|. shiftMask, xK_l     ), sendMessage $ ShrinkFrom R)
      , ((modm .|. altMask .|. shiftMask, xK_h     ), sendMessage $ ShrinkFrom L)
      , ((modm .|. altMask .|. shiftMask, xK_j     ), sendMessage $ ShrinkFrom D)
      , ((modm .|. altMask .|. shiftMask, xK_k     ), sendMessage $ ShrinkFrom U)
      , ((modm .|. altMask,               xK_r     ), sendMessage Rotate)
      , ((modm .|. altMask,               xK_s     ), sendMessage Swap)

      , ((modm,               xK_slash), windowPromptGoto
                                          P.defaultXPConfig { P.autoComplete = Just 500000  })

      , ((modm,               xK_n), namedScratchpadAction scratchpads "ncmpcpp")
      , ((modm,               xK_m), namedScratchpadAction scratchpads "mutt")
      , ((modm .|. shiftMask, xK_h), namedScratchpadAction scratchpads "htop")
      , ((modm,               xK_r), namedScratchpadAction scratchpads "ranger")
      , ((modm,               xK_w), namedScratchpadAction scratchpads "wyrd")

      , ((modm .|. shiftMask, xK_s), do sel <- getSelection
                                        spawn ("echo '" ++ sel ++ "' | ix -i xmonad - | xclip"))
      ]

altMask = mod1Mask
-- end of KEYS }}}


-- ScratchPads {{{
scratchpads :: [NamedScratchpad]
scratchpads = map mkSP ["ncmpcpp", "mutt", "htop", "ranger", "wyrd"]
  where
    mkSP app = NS app ("urxvt -e " ++ app) (title =? app)
                  (customFloating (RationalRect 0 0.5 1 0.5))
-- end of ScratchPads }}}


-- HOOKS {{{
myStartupHook browser = composeAll
    [ spawn     "~/.xmonad/dzenXMonad.sh"
    , spawnOnce browser
    , spawnOnce "pidgin"
    , setWMName "LG3D"
    ]


myManageHook :: ManageHook
myManageHook = composeAll
    [ className   =?  "Pidgin"        --> doShift "2"
    , className   =?  "mplayer2"      --> doFloat
    , className   =?  "mpv"      --> doFloat
    , className   =?  "Steam"         --> doFloat
    , isFullscreen                    --> doFullFloat
    , namedScratchpadManageHook scratchpads
    ]
-- end of HOOKS }}}



-- DZEN {{{
myStatusBar = "dzen2 -x '0' -y '0' -h '13' -w '220' -ta 'l' -bg '" ++ colorBG ++ "' -fg '" ++ colorBG ++ "' -fn '" ++ barFont ++ "'"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor colorFG         colorGreen . hideScratchpad
      , ppVisible           =   dzenColor colorFG         colorRed   . hideScratchpad
      , ppHidden            =   dzenColor colorFG         colorBG    . hideScratchpad
      , ppHiddenNoWindows   =   dzenColor colorBlueLight  colorBG    . hideScratchpad
      , ppUrgent            =   dzenColor colorFG         colorRed   . pad
      , ppWsSep             =   ""
      , ppSep               =   " | "
      , ppTitle             =   const ""
      , ppOutput            =   hPutStrLn h
    }
    where
      hideScratchpad ws = if ws == "NSP" then "" else pad ws -- hide sp in ws list (thanks to p.brisbin)


alert = dzenConfig conf . show . round
  where
    conf = timeout 2
       >=> onCurr (center 20 13)
       >=> font barFont
       >=> addArgs ["-bg", colorBG]
       >=> addArgs ["-fg", colorFG]
-- end of DZEN }}}



-- COLORS, FONTS, AND PROMPTS {{{

-- <colors>
colorFG = colorGrayLight
colorBG = colorBlueDark

colorBlueDark       = "#002b36"
colorBlueLight      = "#586e75"
colorGrayLight      = "#98aeb6"
colorCyan           = "#2aa198"
colorRed            = "#d70000"
colorOrange         = "#cb4b16"
colorGreen          = "#859900"

-- <font>
barFont = "-*-Classical Robot-medium-r-normal--*-80-0-0-p-*-koi6-r"
-- end of COLORS, FONTS, AND PROMPTS }}}



-- LAYOUTS {{{
myLayouts = avoidStruts $ smartBorders $ emptyBSP ||| Full
-- end of LAYOUTS }}}


-- vim:foldmethod=marker foldmarker={{{,}}} sw=2 sts=2 ts=2 tw=0 et ai nowrap
