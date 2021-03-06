import XMonad

main = do
    spawn "sh ~/lib/dot/xmonad/autostart.sh"
    xmonad defaultConfig
           { modMask = mod4Mask
           , terminal = "urxvtc"
           , borderWidth = myBorderWidth
           , focusedBorderColor = myFocusedBorderColor
           }

myBorderWidth = 1
myFocusedBorderColor = "#00FF00"
