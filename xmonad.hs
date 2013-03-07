-- IMPORTS {{{
import XMonad

import System.IO

import qualified Data.Map as M

import Graphics.X11.ExtraTypes.XF86

import XMonad.Util.Run
import XMonad.Util.Dzen
import XMonad.Util.SpawnOnce
import XMonad.Actions.Volume

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog

import XMonad.Layout.Dishes
import XMonad.Layout.ComboP
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Accordion
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
-- end of IMPORTS }}}



-- MAIN {{{
main :: IO ()
main = do
    dzenHandle <- spawnPipe myStatusBar
    xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-fn", barFont, "-bg", colorDarkCream, "-fg", colorBlue] } $ defaultConfig
      { modMask             = mod4Mask
      , terminal            = "urxvt"
      , workspaces          = map show [1..9]
      , keys                = \c -> myKeys c `M.union` keys defaultConfig c
      , layoutHook          = myLayouts
      , manageHook          = myManageHook <+> manageDocks
      , startupHook         = myStartupHook
      , logHook             = myLogHook dzenHandle
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
      , borderWidth         = 1
      , focusFollowsMouse   = True
      }
-- end of MAIN }}}



-- KEYS {{{
myKeys :: XConfig l -> M.Map (ButtonMask, KeySym) (X ())
myKeys XConfig{XMonad.modMask = modm} = M.fromList
      [ ((modm,               xK_asciicircum), spawn "slock")
      , ((modm,               xK_Return     ), spawn "urxvt")
      , ((modm,               xK_p          ), spawn "dmenu_run -b")

      , ((modm,               xK_Up         ), spawn "mpc -q next")
      , ((modm,               xK_Down       ), spawn "mpc -q prev")
      , ((modm,               xK_c          ), spawn "mpc -q toggle")
      , ((modm .|. shiftMask, xK_m          ), spawn "/home/koomi/bin/mpdmenu")

      , ((modm,               xK_u          ), focusUrgent)

      , ((0,         xF86XK_AudioRaiseVolume), raiseVolumeChannels ["Master"] 2 >>= alert)
      , ((0,         xF86XK_AudioLowerVolume), lowerVolumeChannels ["Master"] 2 >>= alert)
      ]
-- end of KEYS }}}



-- HOOKS {{{
myStartupHook = composeAll
    [ spawn     "/home/koomi/.xmonad/dzenXMonad.sh"
    , spawnOnce "dwb"
    , spawnOnce "pidgin"
    , spawnOnce "urxvt -e 'weechat-curses'"
    ]


myManageHook :: ManageHook
myManageHook = composeAll
    [ appName     =?  "weechat 0.4.0" --> doShift "2"
    , className   =?  "Pidgin"        --> doShift "2"
    , className   =?  "Dwb"           --> doShift "1"
    , className   =?  "mplayer2"      --> doFloat
    , className   =?  "Steam"         --> doFloat
    , isFullscreen                    --> doFullFloat
    ]
-- end of HOOKS }}}



-- DZEN {{{
myStatusBar = "dzen2 -x '0' -y '0' -h '14' -w '220' -ta 'l' -bg '" ++ colorDarkGray ++ "' -fg '" ++ colorBlue ++ "' -fn '" ++ barFont ++ "'"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor colorDarkGray   colorCream    . hideScratchpad
      , ppVisible           =   dzenColor colorCream      colorDarkGray . hideScratchpad
      , ppHidden            =   dzenColor colorBlue       colorDarkGray . hideScratchpad
      , ppHiddenNoWindows   =   dzenColor colorGray       colorDarkGray . hideScratchpad
      , ppUrgent            =   dzenColor colorMagenta    colorDarkGray . pad
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
       >=> onCurr (center 20 14)
       >=> font barFont
       >=> addArgs ["-bg", colorDarkGray]
       >=> addArgs ["-fg", colorBlue]
-- end of DZEN }}}



-- COLORS, FONTS, AND PROMPTS {{{

-- <colors>
colorBlack          = "#000000"
colorBlackAlt       = "#040404"
colorGray           = "#444444"
colorGrayAlt        = "#282828"
colorDarkGray       = "#161616"
colorWhite          = "#cfbfad"
colorWhiteAlt       = "#8c8b8e"
colorDarkWhite      = "#606060"
colorCream          = "#a9a6af"
colorDarkCream      = "#5f656b"
colorMagenta        = "#a488d9"
colorMagentaAlt     = "#7965ac"
colorDarkMagenta    = "#8e82a2"
colorBlue           = "#98a7b6"
colorBlueAlt        = "#598691"
colorDarkBlue       = "#464a4a"
colorNormalBorder   = colorGray
colorFocusedBorder  = colorBlue

-- <font>
barFont = "-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-koi8-r"

-- <tab-bar configuration>
myTabTheme =
    defaultTheme { fontName            = barFont
                 , inactiveBorderColor = colorGrayAlt
                 , inactiveColor       = colorDarkGray
                 , inactiveTextColor   = colorGrayAlt
                 , activeBorderColor   = colorGrayAlt
                 , activeColor         = colorDarkMagenta
                 , activeTextColor     = colorDarkGray
                 , urgentBorderColor   = colorBlackAlt
                 , urgentTextColor     = colorWhite
                 , decoHeight          = 14
                 }
-- end of COLORS, FONTS, AND PROMPTS }}}



-- LAYOUTS {{{
myLayouts = smartBorders $ avoidStruts
    $ onWorkspace "2" chatLayout
    $ tall ||| Mirror tall ||| Full ||| Accordion ||| Dishes 2 (1/6)
  where
    tall = Tall 1 (3/100) (1/2)
    chatLayout = combineTwoP (Mirror (TwoPane (3/100) (1/2)))
                              (tabbed shrinkText myTabTheme)
                              Full
                             (ClassName "Pidgin")
-- end of LAYOUTS }}}


-- vim:foldmethod=marker foldmarker={{{,}}} sw=2 sts=2 ts=2 tw=0 et ai nowrap
