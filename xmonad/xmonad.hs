import XMonad

main = xmonad defaultConfig
         { modMask = mod4Mask
         , terminal = "urxvt"
         , borderWidth = 2
         }
