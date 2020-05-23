import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import Data.Word
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Shell ( shellPrompt )
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import System.IO

colourBackground :: String
colourBackground = "#282828"

colourForeground :: String
colourForeground = "#ebdbb2"

highlightBackground :: String
highlightBackground = "#1d2021"

highlightForeground :: String
highlightForeground = "#fabd2f"

-- Set xmobar as my task bar.
myWsBar :: String
myWsBar = "xmobar -v $HOME/.xmobarrc"

myXPConfig = def   -- Configure the prompt's appearance
  { alwaysHighlight   = True
  , bgColor           = colourBackground
  , bgHLight          = highlightBackground
  , fgColor           = colourForeground
  , fgHLight          = highlightForeground
  , font              = "xft:Open Sans:size=9"
  , position          = Top
  , promptBorderWidth = 0
  }

backgroundCmd   = "feh --bg-scale ~/Documents/Images/Posters/FuegoMilkyWay.jpg"
compositeMgr    = "xcompmgr -f -C -n -D 3"
musicCmd        = "mpd"
nextcloudCmd    = "nextcloud"
notificationCmd = "lxqt-notificationd"
screensaverCmd  = "xscreensaver -no-splash"
settingsDaemon  = "xsettingsd"
terminalCmd     = "termonad"
trayerCmd       = "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 7 --transparent true --alpha 1 --tint 0x282828 --height 20 --monitor primary"

main = do
    -- Make sure that HDMI is turned off by default
    spawn "xrandr --output eDP-1 --primary --output HDMI1 --off"
    wsbar <- spawnPipe myWsBar
    xmonad $ desktopConfig
        { focusFollowsMouse = False
        , terminal = terminalCmd   -- Set the default terminal
        , startupHook = do
            spawnOnce screensaverCmd    -- Launch the screen saver
            spawnOnce backgroundCmd     -- Set the background wallpaper
            spawnOnce compositeMgr      -- Launch the composite manager
            spawnOnce settingsDaemon    -- Launch the settings daemon
            spawnOnce trayerCmd         -- Launch the system tray, configured to work correctly with dual monitors
            spawnOnce notificationCmd   -- Launch the notification service
            spawnOnce musicCmd          -- Launch the music playing daemon
            spawnOnce nextcloudCmd      -- Launch the cloud service
        , manageHook      = manageDocks <+> manageHook desktopConfig
        , layoutHook      = avoidStruts $ layoutHook desktopConfig
            ||| spiral (6/7)
            ||| Grid
            ||| Circle
            ||| ThreeCol 1 (3/100) (1/2)
            ||| simpleTabbed
            ||| Accordion
        , handleEventHook = handleEventHook desktopConfig <+> docksEventHook
        , logHook         = dynamicLogWithPP $ xmobarPP
            { ppOutput  = hPutStrLn wsbar
            , ppCurrent = xmobarColor "#859900" "" . wrap "[" "]"
            , ppVisible = xmobarColor "#2aa198" "" . wrap "(" ")"
            , ppLayout  = xmobarColor "#2aa198" ""
            , ppTitle   = xmobarColor "#859900" "" . shorten 50
            }
        , modMask = mod4Mask -- Rebind Mod to the Windows key
        --, borderWidth = 1
        } `additionalKeys`
            -- Use Xmonad's built-in launcher
            [ ((mod4Mask, xK_p), shellPrompt myXPConfig)
            -- Lock the screen
            , ((0, 0x1008ff2d), spawn "xscreensaver-command -lock")
            -- XF86ScreenSaver
            , ((mod4Mask .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
            , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
            , ((0, xK_Print), spawn "scrot")
            -- Turn on the eDP-1 port and set it as the primary display
            , ((mod4Mask .|. shiftMask, xK_e), spawn
                "xrandr --output eDP-1 --primary --auto"
                )
            -- Turn off the HDMI port
            , ((mod4Mask .|. controlMask, xK_h), spawn
                "xrandr --output HDMI-1 --off"
                )
            -- Turn on the HDMI-1 port and set it as the secondary display
            , ((mod4Mask .|. shiftMask, xK_h), spawn
                "xrandr --newmode \"2560x1440R\"  241.50  2560 2608 2640 2720  1440 1443 1448 1481 +hsync -vsync ; xrandr --addmode HDMI-1 2560x1440R ; xrandr --output eDP-1 --primary --auto --output HDMI-1 --above eDP-1 --mode 2560x1440R ; feh --bg-scale ~/Documents/Images/Posters/FuegoMilkyWay.jpg"
                )
            , ((0 , 0x1008FF11), spawn "amixer set Master 2%-") -- XF86AudioLowerVolume
            -- , ((mod4Mask , xK_Down), spawn "amixer set Master 2%-")
            , ((0 , 0x1008FF12), spawn "amixer set Master toggle") -- XF86AudioMute
            , ((0 , 0x1008FF13), spawn "amixer set Master 2%+") -- XF86AudioRaiseVolume
            -- , ((mod4Mask , xK_Up), spawn "amixer set Master 2%+")
            , ((0 , 0x1008FF14), spawn "mpc toggle") -- Play/pause
            -- , ((mod4Mask , xK_End), spawn "mpc toggle")
            , ((mod4Mask .|. controlMask, xK_space), spawn "mpc toggle") -- Play/pause
            , ((0 , 0x1008FF15), spawn "mpc stop") -- Stop
            , ((0 , 0x1008FF16), spawn "mpc prev") -- XF86AudioPrevious
            , ((mod4Mask , xK_Left), spawn "mpc prev") -- prev
            , ((0 , 0x1008FF17), spawn "mpc next") -- XF86AudioNext
            , ((mod4Mask , xK_Right), spawn "mpc next") -- Next
            -- XF86MonBrightnessUp
            , ((0 , 0x1008ff02), spawn "xbrightness +5000")
            -- XF86MonBrightnessDown
            , ((0 , 0x1008ff03), spawn "xbrightness -5000")
            ]
