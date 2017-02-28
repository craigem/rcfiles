import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
        xmproc <- spawnPipe "/usr/bin/xmobar /home/craige/.xmobarrc"
        -- Launch xmobar as my task bar.
        xmonad $ defaultConfig
                { focusFollowsMouse = False
                , terminal = "terminology"
                , manageHook = manageDocks <+> manageHook defaultConfig
                , layoutHook = avoidStruts $ layoutHook defaultConfig
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
                        -- Lock the screen
                        [ ((0, 0x1008ff2d), spawn "xscreensaver-command -lock")
                        -- XF86ScreenSaver
                        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
                        , ((0, xK_Print), spawn "scrot")
                        -- Turn off the display port
                        , ((mod4Mask, xK_d), spawn "/usr/bin/xrandr --output DP-1 --off")
                        -- Turn on the display port and set it as the primary display
                        , ((mod4Mask .|. shiftMask, xK_d), spawn "/usr/bin/xrandr --output DP-1 --primary ; /usr/bin/xrandr --output LVDS-1 --mode 1280x800; /usr/bin/xrandr --output DP-1 --mode 2560x1440; /usr/bin/xrandr --output DP-1 --left-of LVDS-1")
                        -- Turn off the HDMI port
                        , ((mod4Mask .|. controlMask, xK_h), spawn "/usr/bin/xrandr --output HDMI-1 --off")
                        -- Turn on the HDMI port and set it as the primary display
                        , ((mod4Mask .|. shiftMask, xK_h), spawn "/usr/bin/xrandr --output HDMI-1 --primary ; /usr/bin/xrandr --output LVDS-1 --mode 1280x800; /usr/bin/xrandr --output HDMI-1 --mode 1600x900; /usr/bin/xrandr --output HDMI-1 --left-of LVDS-1")
                        , ((mod4Mask, xK_v), spawn "/usr/bin/xrandr --output VGA-1 --off")
                        , ((mod4Mask .|. shiftMask, xK_v), spawn " /usr/bin/xrandr --output VGA-1 --primary ; /usr/bin/xrandr --output LVDS-1 --mode 1280x800; /usr/bin/xrandr --output VGA-1 --mode 1600x900; /usr/bin/xrandr --output VGA-1 --left-of LVDS-1")
                        , ((0 , 0x1008FF11), spawn "amixer set Master 2%-") -- XF86AudioLowerVolume
                        , ((0 , 0x1008FF12), spawn "amixer set Master toggle") -- XF86AudioMute
                        , ((0 , 0x1008FF13), spawn "amixer set Master 2%+") -- XF86AudioRaiseVolume
                        , ((0 , 0x1008FF14), spawn "mpc toggle") -- Play/pause
                        , ((0 , 0x1008FF15), spawn "mpc stop") -- Stop
                        , ((0 , 0x1008FF16), spawn "mpc prev") -- XF86AudioPrevious
                        , ((0 , 0x1008FF17), spawn "mpc next") -- XF86AudioNext
                ]
