import XMonad

main = do
    spawn "sh ~/lib/dot/xmonad/autostart.sh"
    xmonad defaultConfig
           { modMask = mod4Mask
           , terminal = "urxvt"
           , borderWidth = 2
           , focusedBorderColor = myFocusedBorderColor
           }

myFocusedBorderColor = "#00FF00"
