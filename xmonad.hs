-- IMPORTS {{{
import XMonad

import System.IO
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Map as M

import Data.Ratio ((%))

import Graphics.X11.ExtraTypes.XF86

import XMonad.Util.Run
import XMonad.Util.Dzen
import XMonad.Util.Dmenu
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Actions.Volume

import qualified XMonad.Prompt as P
import XMonad.Prompt.Window

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Layout.IM
import XMonad.Layout.Dishes
import XMonad.Layout.ComboP
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Spacing
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile

import XMonad.StackSet (RationalRect(..))
-- end of IMPORTS }}}



-- MAIN {{{
main :: IO ()
main = do
    mpdPort <- newIORef 6600
    hTabBar <- spawnPipe tabLayoutBar
    hStatusBar <- spawnPipe "/home/koomi/.xmonad/dzenXMonad.sh"
    xmonad $ ewmh $ withUrgencyHook NoUrgencyHook
           $ defaultConfig
      { modMask             = mod4Mask
      , terminal            = "urxvt"
      , workspaces          = map show [1..9]
      , keys                = \c -> myKeys hStatusBar mpdPort c `M.union` keys defaultConfig c
      , layoutHook          = myLayouts
      , manageHook          = myManageHook <+> manageDocks
      , startupHook         = myStartupHook
      , logHook             = myLogHook hTabBar
      , normalBorderColor   = colorBlueLight
      , focusedBorderColor  = colorFG
      , borderWidth         = 1
      , focusFollowsMouse   = True
      , handleEventHook     = handleEventHook defaultConfig <+>
                                fullscreenEventHook
      }
-- end of MAIN }}}


-- KEYS {{{
myKeys :: Handle -> IORef Int -> XConfig l -> M.Map (ButtonMask, KeySym) (X ())
myKeys hBar portRef XConfig{XMonad.modMask = modm} = M.fromList
      [ ((modm,               xK_asciicircum), spawn "slock")
      , ((modm,               xK_Return     ), spawn "urxvt")
      , ((modm,               xK_p          ), spawn "dmenu_run -b")

      , ((modm,               xK_Up         ), withPort "mpc -q next -p ")
      , ((modm,               xK_Down       ), withPort "mpc -q prev -p ")
      , ((modm,               xK_c          ), withPort "mpc -q toggle -p ")
      , ((modm .|. shiftMask, xK_m          ), withPort "/home/koomi/bin/mpdmenu ")
      , ((modm,               xK_s          ), portMenu)

      , ((modm,               xK_u          ), focusUrgent)

      , ((0,         xF86XK_AudioRaiseVolume), raiseVolumeChannels ["Master"] 2 >>= alert)
      , ((0,         xF86XK_AudioLowerVolume), lowerVolumeChannels ["Master"] 2 >>= alert)

      , ((modm,               xK_a), sendMessage MirrorShrink)
      , ((modm,               xK_z), sendMessage MirrorExpand)

      , ((modm,               xK_slash), windowPromptGoto
                                          P.defaultXPConfig { P.autoComplete = Just 500000  })

      , ((modm,               xK_n), namedScratchpadAction scratchpads "ncmpcpp")
      , ((modm,               xK_m), namedScratchpadAction scratchpads "mutt")
      , ((modm .|. shiftMask, xK_h), namedScratchpadAction scratchpads "htop")

      ]
  where
      withPort str = do
        p <- liftIO $ readIORef portRef
        spawn $ str ++ show p
      portMenu = do
        p <- fmap read $ menuArgs "dmenu" ["-b"] ["6600", "6601"]
        liftIO $ writeIORef portRef p >> hPutStrLn hBar (show p)
-- end of KEYS }}}


-- ScratchPads {{{
scratchpads :: [NamedScratchpad]
scratchpads = map mkSP ["ncmpcpp", "mutt", "htop"]
  where
    mkSP app = NS app ("urxvt -e " ++ app) (title =? app)
                  (customFloating (RationalRect 0 0.5 1 0.5))
-- end of ScratchPads }}}


-- HOOKS {{{
myStartupHook = composeAll
    [ spawnOnce "dwb"
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
tabLayoutBar = "dzen2 -x '0' -y '0' -h '13' -w '220' -ta 'l' -bg '" ++ colorBG ++ "' -fg '" ++ colorBG ++ "' -fn '" ++ barFont ++ "'"

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

-- <tab-bar configuration>
myTabTheme =
    defaultTheme { activeColor         = "#8a999e"
                 , inactiveColor       = "#545d75"
                 , activeBorderColor   = "white"
                 , inactiveBorderColor = "grey"
                 , activeTextColor     = "white"
                 , inactiveTextColor   = "grey"
                 , decoHeight          = 13
                 , fontName            = barFont
                 }
-- end of COLORS, FONTS, AND PROMPTS }}}



-- LAYOUTS {{{
myLayouts = avoidStruts
    $ onWorkspace "2" (noBorders chatLayout)
    $ smartBorders $ tall ||| Mirror tall ||| Full ||| Dishes 2 (1/6) ||| spaced
  where
    spaced = spacing 30 $ withBorder 5 tall
    tall = ResizableTall 1 (5/100) (1/2) []
    chatLayout = combineTwoP (Mirror (TwoPane (3/100) (1/2)))
                              pidgin
                              Full
                             (ClassName "Pidgin")
    pidgin = withIM (1%6) (Title "Buddy List") (tabbed shrinkText defaultTheme)
-- end of LAYOUTS }}}


-- vim:foldmethod=marker foldmarker={{{,}}} sw=2 sts=2 ts=2 tw=0 et ai nowrap
