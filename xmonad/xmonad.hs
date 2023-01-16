{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import XMonad
  ( ChangeLayout (NextLayout),
    Default (def),
    IncMasterN (IncMasterN),
    KeyMask,
    ManageHook,
    Resize (Expand, Shrink),
    X,
    XConfig (borderWidth, layoutHook, logHook, manageHook, modMask, startupHook, terminal),
    XState (windowset),
    appName,
    className,
    composeAll,
    doFloat,
    gets,
    io,
    mod1Mask,
    mod4Mask,
    resource,
    screenWorkspace,
    sendMessage,
    spawn,
    stringProperty,
    title,
    whenJust,
    windows,
    withFocused,
    xmonad,
    (<+>),
    (=?),
  )
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (WSType (..), anyWS, moveTo, nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, shiftTo, toggleWS)
import XMonad.Actions.FloatKeys (keysMoveWindow, keysResizeWindow)
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts)
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook, swapWithLast, toggleFocus)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Layout.Groups.Helpers (swapMaster)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import XMonad.Layout.MultiToggle qualified as MT
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL))
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile (MirrorResize (MirrorExpand, MirrorShrink), ResizableTall (ResizableTall))
import XMonad.Layout.ShowWName (showWName')
import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Layout.Spacing (Border (Border), Spacing, decScreenSpacing, decWindowSpacing, incScreenSpacing, incWindowSpacing, spacingRaw)
import XMonad.Layout.ToggleLayouts qualified as T
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.ManageHook ((-->))
import XMonad.Prompt (Direction1D (Next, Prev))
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad (NamedScratchpad (..), customFloating, namedScratchpadAction, namedScratchpadManageHook, toggleDynamicNSP, dynamicNSPAction)
import XMonad.Util.Run (safeSpawn, spawnPipe)

main :: IO ()
main = do
  xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
  xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc"
  xmproc2 <- spawnPipe "xmobar -x 2 $HOME/.config/xmobar/xmobarrc"

  let conf =
        def
          { modMask = myModMask,
            terminal = myTerminal,
            startupHook = myStartupHook,
            manageHook = myManageHook <+> namedScratchpadManageHook scratchpads,
            layoutHook = refocusLastLayoutHook $ showWName' def myLayoutHook,
            borderWidth = 0,
            logHook =
              workspaceHistoryHook
                <+> dynamicLogWithPP
                  xmobarPP
                    { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x,
                      ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]", -- Current workspace in xmobar
                      ppVisible = xmobarColor "#98be65" "", -- Visible but not current workspace
                      ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "", -- Hidden workspaces in xmobar
                      ppHiddenNoWindows = xmobarColor "#c792ea" "", -- Hidden workspaces (no windows)
                      ppTitle = xmobarColor "#b3afc2" "" . shorten 60, -- Title of active window in xmobar
                      ppSep = "<fc=#666666> <fn=1>|</fn> </fc>", -- Separators in xmobar
                      ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!", -- Urgent workspace
                      ppExtras = [windowCount], -- # of windows current workspace
                      ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                    }
          }

  xmonad $ ewmhFullscreen $ ewmh $ conf `additionalKeysP` myKeyBindings

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty" -- Sets default terminal

myBrowser :: String
myBrowser = "brave"

myScreenLocker :: String
myScreenLocker = "i3lock-fancy-rapid 10 pixel"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs --fg-daemon'" -- Sets emacs as editor for tree select
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select

myEmail :: String
myEmail = "emacsclient -c -a emacs --eval '(notmuch)'"

myRofi :: String
myRofi = "rofi -modi drun,ssh,window -show drun -show-icons"

myDmenu :: String
myDmenu = "dmenu_run"

altMask :: KeyMask
altMask = mod1Mask -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook =
  do
    spawn "xset r rate 350 80"
    spawn "picom -cf -i 0.8 --use-ewmh-active-win"
    <+> setWMName "LG3D"

myLayoutHook = avoidStruts $ mouseResize myDefaultLayout
  where
    myDefaultLayout = renamed [Replace "tall"] (mkToggleAll tall)
    mkToggleAll l =
      windowArrange
        $ T.toggleLayouts floats
        $ mkToggle (NBFULL ?? EOT)
          . mkToggle (single MIRROR)
        $ l
    tall = mkLayout $ ResizableTall 1 (3 / 100) (1 / 2) []
    floats = mkLayout simplestFloat
    mkLayout layout = windowNavigation $ mySpacing 6 layout
      where
        mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
        mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myManageHook :: ManageHook
myManageHook =
  composeAll . concat $
    [ [resource =? t --> doFloat | t <- byResource],
      [className =? c --> doFloat | c <- byClass],
      [title =? t --> doFloat | t <- byTitle],
      [stringProperty "WM_WINDOW_ROLE" =? r --> doFloat | r <- byRole],
      [stringProperty "WM_WINDOW_TYPE" =? t --> doFloat | t <- byType],
      [isDialog --> doFloat]
    ]
  where
    byResource = ["Devtools", "plasmashell"]
    byClass = ["org.gnome.Nautilus", "Gimp"]
    byTitle = ["Open Document", "Open Files", "Developer Tools"]
    byRole = ["pop-up", "GtkFileChooserDialog", "bubble"]
    byType = ["_NET_WM_WINDOW_TYPE_SPLASH", "_NET_WM_WINDOW_TYPE_DIALOG"]

data App
  = ClassApp {name :: String, px :: Rational, py :: Rational, wd :: Rational, ht :: Rational, cmd :: String}
  | TitleApp {name :: String, px :: Rational, py :: Rational, wd :: Rational, ht :: Rational, cmd :: String}
  | NameApp {name :: String, px :: Rational, py :: Rational, wd :: Rational, ht :: Rational, cmd :: String}
  deriving (Show)

scratchpads :: [NamedScratchpad]
scratchpads =
  mkNS
    <$> [ TitleApp "emacs" (10 / 32) (1 / 32) (20 / 32) (30 / 32) "emacsclient -s emacs -c -a 'emacs --title emacs --bg-daemon=emacs'",
          TitleApp "tmux" (4 / 32) (1 / 32) (24 / 32) (30 / 32) (myTerminal ++ " -t tmux -e tmux"),
          TitleApp "htop" (1 / 32) (1 / 32) (30 / 32) (16 / 32) (myTerminal ++ " -t htop -e htop"),
          TitleApp "btm" (16 / 32) (1 / 32) (15 / 32) (30 / 32) (myTerminal ++ " -t btm -e btm"),
          ClassApp "Brave-browser" (10 / 32) (1 / 32) (20 / 32) (30 / 32) myBrowser
        ]
  where
    mkNS TitleApp {..} = NS name cmd (title =? name) (customFloating $ W.RationalRect px py wd ht)
    mkNS ClassApp {..} = NS name cmd (className =? name) (customFloating $ W.RationalRect px py wd ht)
    mkNS NameApp {..} = NS name cmd (appName =? name) (customFloating $ W.RationalRect px py wd ht)

------------------------------------------------------------------------
-- Key Bindings
myKeyBindings =
  [ -- ("M-q"       , spawn "xmonad --recompile; xmonad --restart")
    -- ("M-q"       , spawn "restart-xmonad.sh")
    ("M-C-q", spawn "xmonad-restart"),
    ("M-C-S-q", io exitSuccess), -- Quits xmonad

    -- Launch programs
    ("M-p", spawn myDmenu),
    ("M-S-p", spawn myRofi),
    -- ("M-s", spawn "dm-search.sh"),
    ("M-v", spawn "clipmenu"),
    ("M-c", spawn "mkdir -p ~/captures; flameshot gui -p ~/captures/"),
    -- , ("M-o"                    , spawn "dmenu_run -i -p \"Run: \"")
    ("M-/", spawn "dm-qutebrowser-history.sh"),
    -- Windows navigation
    ("M-S-m", swapMaster), -- Moves focused window to master, others maintain order
    ("M-C-<Tab>", rotAllDown), -- Rotate all the windows in the current stack
    ("M-C-S-<Tab>", rotSlavesDown), -- Rotate all windows except master and keep focus in place
    ("M-n", toggleFocus), -- Move focus to the lastly focused
    ("M-S-n", swapWithLast), -- Move the focused to the lastly focused

    -- Kill windows
    ("M-S-c", kill1), -- Kill the currently focused client
    ("M-C-S-c", killAll), -- Kill all windows on current workspace

    -- Workspaces
    ("M-[", moveTo Prev anyWS), -- moveTo previous workspace
    ("M-]", moveTo Next anyWS), -- moveTo next workspace
    ("M-`", toggleWS),
    ("M-S-[", shiftTo Prev nonNSP >> moveTo Prev nonNSP), -- Shifts focused window to prev ws and move
    ("M-S-]", shiftTo Next nonNSP >> moveTo Next nonNSP), -- Shifts focused window to next ws and move
    ("M-C-[", prevScreen), -- Switch focus to prev monitor
    ("M-C-]", nextScreen), -- Switch focus to next monitor
    ("M-C-S-[", shiftPrevScreen >> prevScreen), -- Shifts focused window to prev monitor and move
    ("M-C-S-]", shiftNextScreen >> nextScreen), -- Shifts focused window to next monitor and move

    -- Floating windows
    ("M-t", withFocused $ windows . W.sink), -- Push floating window back to tile
    ("M-S-t", sinkAll), -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
    ("M--", decWindowSpacing 1), -- Decrease window spacing
    ("M-=", incWindowSpacing 1), -- Increase window spacing
    ("M-S--", decScreenSpacing 1), -- Decrease screen spacing
    ("M-S-=", incScreenSpacing 1), -- Increase screen spacing

    -- Layouts
    ("M-<Space>", sendMessage NextLayout),
    ("M-r", sendMessage $ MT.Toggle MIRROR),
    ("M-C-M1-<Up>", sendMessage Arrange),
    ("M-C-M1-<Down>", sendMessage DeArrange),
    ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggles noborder/full
    ("M-S-f", sendMessage ToggleStruts),
    ("M-C-f", sendMessage (T.Toggle "floats")), -- Toggles my 'floats' layout

    -- Increase/decrease windows in the master pane or the stack
    ("M-,", sendMessage (IncMasterN 1)), -- Increase number of clients in master pane
    ("M-.", sendMessage (IncMasterN (-1))), -- Decrease number of clients in master pane
    -- , ("M-C-,"        , increaseLimit)                   -- Increase number of windows
    -- , ("M-C-."        , decreaseLimit)                   -- Decrease number of windows

    -- Window resizing
    ("M-C-h", sendMessage Shrink), -- Shrink horiz window width
    ("M-C-l", sendMessage Expand), -- Expand horiz window width
    ("M-C-j", sendMessage MirrorShrink), -- Shrink vert window width
    ("M-C-k", sendMessage MirrorExpand), -- Exoand vert window width
    ("M-C-i", withFocused $ keysResizeWindow (0, 9) (1 / 2, 1 / 2)),
    ("M-C-u", withFocused $ keysResizeWindow (0, -9) (1 / 2, 1 / 2)),
    ("M-C-o", withFocused $ keysResizeWindow (16, 0) (1 / 2, 1 / 2)),
    ("M-C-y", withFocused $ keysResizeWindow (-16, 0) (1 / 2, 1 / 2)),
    -- Window moving
    ("M-i", withFocused $ keysMoveWindow (0, -9)),
    ("M-u", withFocused $ keysMoveWindow (0, 9)),
    ("M-o", withFocused $ keysMoveWindow (16, 0)),
    ("M-y", withFocused $ keysMoveWindow (-16, 0)),
    -- emacs in tiling
    ("M-S-d", spawn myEditor),
    ("M-S-<Return>", spawn $ myTerminal ++ " -e tmux"),
    ("M-C-<Return>", namedScratchpadAction scratchpads "tmux"),
    -- Scratchpads
    ("M-C-d", namedScratchpadAction scratchpads "Brave-browser"),
    ("M-d", namedScratchpadAction scratchpads "emacs"),
    ("M-z", namedScratchpadAction scratchpads "htop"),
    ("M-x", namedScratchpadAction scratchpads "btm"),
    -- Dynamic Scratchpads
    ("M-S-a", withFocused $ toggleDynamicNSP "dyn1"),
    ("M-S-s", withFocused $ toggleDynamicNSP "dyn2"),
    ("M-a", dynamicNSPAction "dyn1"),
    ("M-s", dynamicNSPAction "dyn2"),
    -- environment
    ("M-M1-9", spawn "xbacklight -inc 5"),
    ("M-M1-8", spawn "xbacklight -dec 5"),
    -- Multimedia Keys
    ("<XF86AudioPlay>", spawn (myTerminal ++ " mocp --play")),
    ("<XF86AudioPrev>", spawn (myTerminal ++ " mocp --previous")),
    ("<XF86AudioNext>", spawn (myTerminal ++ " mocp --next")),
    ("<XF86AudioMute>", spawn "amixer set Master toggle"),
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute"),
    ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5"),
    ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5"),
    ("<XF86Favorites>", spawn myScreenLocker),
    ("<F12>", spawn myScreenLocker),
    ("M-M1-C-S-l", spawn myScreenLocker),
    ("<XF86HomePage>", spawn myBrowser),
    ("<XF86Search>", safeSpawn myBrowser ["https://hoogle.hackage.org"]),
    ("<XF86Mail>", spawn myEmail),
    ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk")),
    ("<XF86Eject>", spawn "toggleeject"),
    ("<Print>", spawn "scrotd 0")
  ]
    -- screen view and shift
    ++ [ ("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
         | (k, sc) <- zip ["q", "w", "e"] [0 ..],
           (f, m) <- [(W.view, ""), (W.shift, "S-")]
       ]
  where
    -- The following lines are needed for named scratchpads.

    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
    nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
