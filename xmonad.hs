import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns

import XMonad.Hooks.EwmhDesktops

import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.Run (runProcessWithInput)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn y = filter (not . null) . go y where
  go :: Eq a => a -> [a] -> [[a]]
  go _ [] = []
  go y xs = takeWhile (/= y) xs : go y (tail $ dropWhile (/= y) xs)

currKeyLayout :: IO String
currKeyLayout = lastWord . lastLine <$> layout where
  layout = runProcessWithInput "setxkbmap" ["-query"] ""
  lastLine = last . splitOn '\n'
  lastWord = last . words


switchKeyLayout :: String -> IO ()
switchKeyLayout lang = spawn $ "setxkbmap -layout " ++ next lang where
  next "us" = "latam"
  next _    = "us"

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

{-
Check out the names of keys in your system at: /usr/include/X11/XF86keysym.h
Check out the EZConfig key descriptors in the documentation: https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Util-EZConfig.html

tu cara :]
-}


myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    }
  `additionalKeys`
  [ ((0, 0x1008FF13), spawn "amixer -q sset Master 2%+") -- Enable key to increase volume by 22%
  , ((0, 0x1008FF11), spawn "amixer -q sset Master 2%-") -- Enable key to decrease volume by 2%
  , ((mod4Mask .|. shiftMask, xK_l), liftIO $ currKeyLayout >>= switchKeyLayout) -- Rotate between keyboard layouts
  , ((mod4Mask .|. shiftMask, xK_s), spawn "mate-screenshot --interactive") -- Spawn screenshot tool
  , ((mod4Mask, xK_a), spawn "alacritty") -- Spawn alacritty emulator
  ]
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock") -- Spawn screensaver
    , ("M-S-=", unGrab *> spawn "scrot -s"        ) -- Take screenshot
    , ("M-f", spawn "firefox") -- spawn firefox
    , ("<XF86MonBrightnessUp>", spawn "lux -a 10%") -- Enable key to increase brightness by 10%
    , ("<XF86MonBrightnessDown>", spawn "lux -s 10%") -- Enable key to decrease brightness by 10%
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle") -- Mute/unmute audio
    --, ("<xF86XK_AudioRaiseVolume>", spawn "amixer -q sset Master 2%+") -- Enable key to increase volume by 2%
    --, ("<xF86XK_AudioLowerVolume>", spawn "amixer -q sset Master 2%-") -- Enable key to decrease volume by 2%
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""