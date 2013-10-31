import XMonad 

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook

import XMonad.StackSet (swapUp, swapDown, shiftMaster)

import XMonad.Util.Run(spawnPipe)

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.Named
import XMonad.Layout.Circle
import XMonad.Layout.Gaps
--import XMonad.Layout.StackTile

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Man

import XMonad.Util.Scratchpad

import qualified XMonad.StackSet as S

import System.IO
import Data.Ratio ((%))
import qualified Data.Map as M

-- for xmobarStrip
import Data.List (isPrefixOf)

-- for dzen2
--import XMonad.Util.Dzen


wmName       = stringProperty "WM_NAME"
wmWindowRole = stringProperty "WM_WINDOW_ROLE"

floatClass = []
floatTitle = []
hacking    = ["Happy Hacking"]
coding     = ["Happy Coding"]
webApps    = ["Firefox", "Google-chrome", "Chromium"]
comApps    = ["Pidgin", "jabber", "Jabber", "Empathy"]
mailApps   = ["OUTLOOK.EXE", "Wine", "mutt", "mail", "evolution", "Evolution"]
gimpApp    = ["Gimp", "gimp"]
skypeApp   = ["Skype", "skype", "MainWindow"]
ircApps    = ["workies", "irc"]

wmSkypeMain x = do x1 <- className    =? x
                   x2 <- wmWindowRole =? "MainWindow"
                   return (x1 && x2)

myWorkspaces = map show [1] ++ ["code", "web", "im", "mail", "skype", "gimp", "musi", "irc"]

curLayout :: X String
curLayout = gets windowset >>= return . description . S.layout . S.workspace . S.current

main = do
     	 spawnPipe "/bin/bash /home/frosch03/.xstartup"
--       xmobar <- spawnPipe "/usr/local/bin/xmobar"
--       dzen2 <- spawnPipe "/usr/local/bin/dzen2 -ta l -fg grey80 -bg grey20 -e 'button3='"
         xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
            { borderWidth        = 1
            , normalBorderColor  = "#333333" -- previous "#003300"
            , focusedBorderColor = "#999999" -- previous "#00ff00"
            , modMask            = mod4Mask
            , keys               = newKeys
            , workspaces         = myWorkspaces
            , manageHook         = manageDocks <+> manageHook defaultConfig <+> myManagedHook
            , layoutHook         = myLayout
            , logHook            = (dynamicLogWithPP $ myDzen2PP { ppOutput  = \x -> writeFile "/tmp/dmpipe1" $ "1 " ++ x ++ "\n"})
            } 


-- The pretty printed layout for my xmobar 
myXmobarPP = defaultPP 
                 { ppTitle   = xmobarColor "#6080ff" "" . shorten 50 
                 , ppCurrent = xmobarColor "#ffffff" "" . wrap "[" "]" 
                 , ppVisible = xmobarColor "#999999" "" . wrap "(" ")"
                 , ppUrgent  = xmobarColor "#c04040" "" . xmobarStrip
                 , ppHidden  = xmobarColor "#000000" ""
                 , ppSep     = " | "
                 } 
                 
myDzen2PP = defaultPP 
                 { ppTitle   = dzenColor "#61ce3c" "" . dzenEscape
                 , ppCurrent = dzenColor "#f8f8f8" "DodgerBlue4" . pad
                 , ppVisible = dzenColor "#f8f8f8" "LightSkyBlue4" . pad
                 , ppUrgent  = dzenColor "#f8f8f8" "red4" . pad . dzenStrip
                 , ppHidden  = pad
                 , ppSep     = " "
                 , ppWsSep   = "|"
--               , ppLayout  = dzenColor "DarkOrange" "" . wrap "^ca(1,xdotool key alt+space)[" "]^ca()"
                 , ppLayout  = (wrap "^ca(1,xdotool key alt+space)^fg(darkorange)|^fg(grey)" "^fg(darkorange)|^ca()") . 
                               (\x -> case x of
                                "Tall"                -> "^i(/home/frosch03/.dzen2/dzenIcons/xbm8x8/layout_tall.xbm)"
                                "Mirror Tall"         -> "^i(/home/frosch03/.dzen2/dzenIcons/xbm8x8/layout_mirror_tall.xbm)"
                                "Full"                -> "^i(/home/frosch03/.dzen2/dzenIcons/xbm8x8/layout_full.xbm)"
                                "Row Code"            -> "^i(/home/frosch03/.dzen2/dzenIcons/xbm8x8/layout_row_code.xbm)"
                                "Column Code"         -> "^i(/home/frosch03/.dzen2/dzenIcons/xbm8x8/layout_column_code.xbm)"
                                "ReflectX IM Grid"    -> "^i(/home/frosch03/.dzen2/dzenIcons/xbm8x8/layout_reflectx_im_grid.xbm)"
                                "IM ReflectX IM Full" -> "^i(/home/frosch03/.dzen2/dzenIcons/xbm8x8/layout_im_reflectx_im_full.xbm)"
                               )

                 }
              --    (\x -> case x of
              --       "Tall" -> "^i(/home/dk/.icons/dzen2/tall.xbm)"
              --        "Mirror Tall" -> "^i(/home/dk/.icons/dzen2/mtall.xbm)"
              --        "Full" -> "^i(/home/dk/.icons/dzen2/full.xbm)"
              --    )


-- The layoutdefinition for my workspaces
myLayout  = smartBorders 
	  $ avoidStruts 
          $ onWorkspaces ["1"]      (Full ||| tiled ||| Mirror tiled)
          $ onWorkspaces ["code"]   (codeColumn ||| codeRow)
          $ onWorkspaces ["web"]    (Full ||| (gaps [(L,250), (R,250)] $ Full))
          -- $ onWorkspaces ["im"]     (Full)
          $ onWorkspaces ["im"]     (   reflectHoriz (pidginLayout gridLayout)
                                    ||| reflectHoriz (pidginLayout Full)
                                    ||| reflectHoriz (pidginLayout Circle)
--                                    ||| reflectHoriz (pidginLayout (StackTile 1 (10/100) (90/100)))
                                    )
	  $ onWorkspaces ["mail"]   (Full)
	  $ onWorkspaces ["skype"]  (reflectHoriz $ (withIM (1%6) (Role "MainWindow") Grid))
 	  $ onWorkspaces ["gimp"]   (gimp)
       	  $ onWorkspaces ["musi"]   (Circle)
	  $ tiled ||| Mirror tiled ||| Full ||| Circle
    where tiled   = Tall nmaster delta ratio
          nmaster = 1 
          ratio   = 1/2
          delta   = 3/100
          codeColumn = named "Column Code" $ Tall nmaster delta' ratio'
          codeRow    = named "Row Code" $ Mirror $ reflectHoriz $ Tall nmaster delta' ratio'
          ratio'     = 1/3
          delta'     = 0
          gridLayout = spacing 8 $ Grid      
          pidginLayout l = withIM (18/100) (Role "buddy_list") l
          gimp       = withIM (0.11) (Role "gimp-toolbox") $
                       reflectHoriz $
                       withIM (0.15) (Role "gimp-dock") Full

-- The XPConfig definition for my XMonad.Prompts 
myXPConfig = defaultXPConfig { bgColor     = "black"
                             , fgColor     = "white"
                             , borderColor = "#333333" -- previous "green"
                             , position    = Top
                             , font        = "xft:SourceCodePro-Regular:pixelsize=8:autohint=true"
                             }

xK_XF86Play = 0x1008FF14
xK_XF86Stop = 0x1008FF15
xK_XF86Fwrd = 0x1008FF17
xK_XF86Bwrd = 0x1008FF16
xK_XF86Thnk = 0x1008FF41
xK_XF86Sleep            = 0x1008ff2f
xK_XF86ScreenSaver      = 0x1008ff2d
xK_XF86AudioRaiseVolume = 0x1008ff13
xK_XF86AudioLowerVolume = 0x1008ff11
xK_FroggersPause = 0x1008ff12


-- My additional keybindings
myKeys x = M.fromList $
  [ ((modMask x,                 xK_p), shellPrompt myXPConfig)
  , ((modMask x .|. shiftMask,   xK_r), spawn "emacsclient -c -e '(frog-remember-frame)'")
  , ((modMask x,                 xK_y), scratchpadSpawnActionTerminal "urxvt")
  , ((modMask x,                 xK_m), spawn "/usr/bin/dmpc")
  , ((modMask x .|. shiftMask,   xK_m), manPrompt myXPConfig)
  , ((modMask x .|. shiftMask,   xK_s), sshPrompt myXPConfig)
  , ((modMask x .|. shiftMask,   xK_f), focusUrgent)
  , ((modMask x .|. shiftMask,   xK_j), windows swapDown)
  , ((modMask x .|. shiftMask,   xK_k), windows swapUp)
  , ((modMask x,                 xK_m), windows shiftMaster)
  , ((modMask x .|. shiftMask,   xK_Return), spawn "/usr/bin/urxvt")
  , ((modMask x .|. controlMask, xK_l),	spawn "gnome-screensaver-command --lock")
  , ((modMask x .|. controlMask, xK_a),	spawn "/home/frosch03/whatIsThisPlace")
  , ((0        ,          xK_XF86Play), spawn "/usr/bin/mpc toggle")
  , ((0        ,          xK_XF86Stop), spawn "/usr/bin/mpc stop")
  , ((0        ,          xK_XF86Fwrd), spawn "/usr/bin/mpc next")
  , ((0        ,          xK_XF86Bwrd), spawn "/usr/bin/mpc prev")
  , ((0        ,          xK_XF86Thnk), spawn "/home/frosch03/whatIsThisPlace")
  , ((0        ,         xK_XF86Sleep),	spawn "sudo pm-suspend")
  , ((0        ,   xK_XF86ScreenSaver),	spawn "gnome-screensaver-command --lock")
  , ((0      ,xK_XF86AudioRaiseVolume),	spawn "/home/frosch03/.xmonad/volUp.sh")
  , ((0      ,xK_XF86AudioLowerVolume),	spawn "/home/frosch03/.xmonad/volDn.sh")
  -- , ((0      ,xK_XF86AudioRaiseVolume),	spawn "pacmd set-sink-volume 0 $(printf '0x%x' $(( $(pacmd dump|grep set-sink-volume|cut -f3 -d' ') + 0xf00)) )")
  -- , ((0      ,xK_XF86AudioLowerVolume),	spawn "pacmd set-sink-volume 0 $(printf '0x%x' $(( $(pacmd dump|grep set-sink-volume|cut -f3 -d' ') - 0xf00)) )")
  ]
newKeys x = myKeys x `M.union` keys defaultConfig x

-- My additional managed applications (browser is always on desktop 3 and in fullscreen, etc.)
myManagedHook = composeAll . concat $
  [ [ className =? c --> doFloat               | c <- floatClass ]
  , [ title     =? t --> doFloat               | t <- floatTitle ]
  , [ title     =? x --> doF (S.shift "1")     | x <- hacking]
  , [ title     =? x --> doF (S.shift "code")  | x <- coding]
  , [ className =? x --> doF (S.shift "web")   | x <- webApps ]
  , [ className =? x --> doF (S.shift "im")    | x <- comApps ]
  , [ title     =? x --> doF (S.shift "im")    | x <- comApps ]
  , [ className =? x --> doF (S.shift "mail")  | x <- mailApps ]
  , [ title     =? x --> doF (S.shift "mail")  | x <- mailApps ]
  , [ className =? x --> doF (S.shift "gimp")  | x <- gimpApp ]
  , [ className =? x --> doF (S.shift "skype") | x <- skypeApp ]
  , [ title     =? x --> doF (S.shift "irc")   | x <- ircApps ]
  , [ isFullscreen   --> doFullFloat]
  ]

----------------------------------------------------------------------------
-- copy from DynamicLog-1.hs because xmobarStrip exists only in the 
-- darcs-version of xmobar ... 
-- propably i have to change this some time ... 
----------------------------------------------------------------------------

-- ??? add an xmobarEscape function?

-- | Strip xmobar markup. Useful to remove ppHidden color from ppUrgent
--   field. For example:
--
-- >     , ppHidden          = xmobarColor "gray20" "" . wrap "<" ">"
-- >     , ppUrgent          = xmobarColor "dark orange" "" .  xmobarStrip
-- xmobarStrip :: String -> String
-- xmobarStrip = strip [] where
--     strip keep x
--       | null x                 = keep
--       | "<fc="  `isPrefixOf` x = strip keep (drop 1 . dropWhile (/= '>') $ x)
--       | "</fc>" `isPrefixOf` x = strip keep (drop 5  x)
--       | '<' == head x          = strip (keep ++ "<") (tail x)
--       | otherwise              = let (good,x') = span (/= '<') x
--                                  in strip (keep ++ good) x'
