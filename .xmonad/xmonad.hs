import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import Data.Word
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
--import XMonad.Util.SpawnOnce
import System.IO

main = do
    -- Make sure that HDMI is turned off by default
    spawn "xrandr --output HDMI1 --off"
    spawn "xrandr --newmode \"2560x1440R\"  241.50  2560 2608 2640 2720  1440 1443 1448 1481 +hsync -vsync"
    spawn "xrandr --addmode HDMI-1 2560x1440R && xrandr --output eDP-1 --auto --output HDMI-1 --primary --above eDP-1 --mode 2560x1440R"
    -- Launch the composite manager
    spawn "xcompmgr -f -C -n -D 3"
    -- Launch xmobar as my task bar.
    xmproc <- spawnPipe "xmobar /home/craige/.xmobarrc"
    -- Launch the system tray
    spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand false --width 5 --transparent true --alpha 0 --tint 0x073642 --height 20 --monitor primary"
    -- Launch the settings daemon
    spawn "xsettingsd"
    -- Launch the screen saver
    spawn "xscreensaver -no-splash"
    spawn "nm-applet"
    spawn "owncloud"
    -- Launch the music playing daemon
    spawn "mpd"
    -- Set the background wallpaper
    spawn "feh --bg-scale ~/Documents/Images/Posters/FuegoMilkyWay.jpg"
    spawn "lxqt-notificationd"
    xmonad $ desktopConfig
        { focusFollowsMouse = False
        , terminal = "alacritty"
        , manageHook = manageDocks <+> manageHook desktopConfig
        , layoutHook = avoidStruts $ layoutHook desktopConfig
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppCurrent = xmobarColor "#859900" "" . wrap "[" "]"
            , ppVisible = xmobarColor "#2aa198" "" . wrap "(" ")"
            , ppLayout = xmobarColor "#2aa198" ""
            , ppTitle = xmobarColor "#859900" "" . shorten 50
            }
        , modMask = mod4Mask -- Rebind Mod to the Windows key
        --, borderWidth = 1
        } `additionalKeys`
            -- Custom dmenu launcher
            [ ((mod4Mask, xK_p ), spawn
                " exe=`dmenu_path | dmenu -fn \"Open Sans-10\" -p \"λ:\" \
                \ -nb \"#073642\" -nf \"#93a1a1\" -sb \"#002b36\" -sf \
                \ \"#859900\"` && eval \"exec $exe\""
                )
            -- Lock the screen
            , ((0, 0x1008ff2d), spawn "xscreensaver-command -lock")
            -- XF86ScreenSaver
            , ((mod4Mask .|. controlMask, xK_l), spawn "xscreensaver-command -lock")
            , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
            , ((0, xK_Print), spawn "scrot")
            -- Turn on the eDP-1 port and set it as the primary display
            , ((mod4Mask .|. shiftMask, xK_e), spawn
                "xrandr --output eDP-1 --primary --auto --output "
                )
            -- Turn off the HDMI port
            , ((mod4Mask .|. controlMask, xK_h), spawn
                "xrandr --output HDMI-1 --off"
                )
            -- Turn on the HDMI-1 port and set it as the secondary display
            , ((mod4Mask .|. shiftMask, xK_h), spawn
                "xrandr --output eDP-1 --primary --auto --output HDMI-1 --above eDP-1 --mode 2560x1440R"
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
