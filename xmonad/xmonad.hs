{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}


import XMonad
    ( mod1Mask,
      mod4Mask,
      gets,
      io,
      spawn,
      whenJust,
      whenX,
      xmonad,
      (<&&>),
      (<+>),
      (<||>),
      (=?),
      appName,
      className,
      doFloat,
      doIgnore,
      resource,
      stringProperty,
      title,
      screenWorkspace,
      sendMessage,
      windows,
      withFocused,
      KeyMask,
      Default(def),
      Query,
      WindowSet,
      X,
      XConfig(logHook, manageHook, modMask, terminal, startupHook,
              layoutHook, workspaces, borderWidth, normalBorderColor,
              focusedBorderColor),
      XState(windowset),
      ChangeLayout(NextLayout),
      Full(Full),
      IncMasterN(IncMasterN),
      JumpToLayout(JumpToLayout),
      Mirror(Mirror),
      Resize(Expand, Shrink) )
import System.Directory ( getHomeDirectory )
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS ( moveTo, shiftTo, WSType(..)
                              , nextScreen, prevScreen
                              , shiftPrevScreen, shiftNextScreen
                              , toggleWS, anyWS
                              )
import XMonad.Actions.MouseResize ( mouseResize )
import XMonad.Actions.SwapPromote (swapHybrid)
import XMonad.Actions.DwmPromote (dwmpromote)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.SpawnOn ( manageSpawn, spawnOn )

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe ( fromJust, isJust )
import Data.Monoid ( Endo )
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops ( ewmhFullscreen )  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive ( fadeInactiveLogHook )
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Hooks.WorkspaceHistory ( workspaceHistoryHook )
import XMonad.Hooks.RefocusLast
    ( refocusLastLayoutHook, swapWithLast, toggleFocus )
import XMonad.Hooks.ManageHelpers            ( (-?>)
                                             , composeOne
                                             , doCenterFloat
                                             , doFullFloat
                                             , isDialog
                                             , isFullscreen
                                             , isInProperty
                                             )
import XMonad.Hooks.InsertPosition ( Focus(Newer), Position(Below), insertPosition)
    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat ( simplestFloat )
import XMonad.Layout.Spiral ( spiral )
import XMonad.Layout.ResizableTile
    ( MirrorResize(MirrorExpand, MirrorShrink),
      ResizableTall(ResizableTall) )
import XMonad.Layout.Tabbed
    ( Direction2D(D, L, R, U),
      shrinkText,
      addTabs,
      Theme(inactiveTextColor, fontName, activeColor, inactiveColor,
            activeBorderColor, inactiveBorderColor, activeTextColor) )
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeCol) )
import XMonad.Layout.LayoutCombinators ( (|||) )
import XMonad.Layout.Accordion ( Accordion(Accordion) )

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier ( ModifiedLayout )
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Reflect
    ( REFLECTX(REFLECTX), REFLECTY(REFLECTY) )
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders ( noBorders, smartBorders )
import XMonad.Layout.Renamed ( renamed, Rename(Replace) )
import XMonad.Layout.ShowWName
    ( showWName',
      SWNConfig(swn_color, swn_font, swn_fade, swn_bgcolor) )
import XMonad.Layout.Simplest ( Simplest(Simplest) )
import XMonad.Layout.Spacing
    ( decScreenSpacing,
      decWindowSpacing,
      incScreenSpacing,
      incWindowSpacing,
      spacingRaw,
      Border(Border),
      Spacing )
import XMonad.Layout.SubLayouts
    ( onGroup,
      pullGroup,
      subLayout,
      toSubl,
      GroupMsg(UnMergeAll, MergeAll, UnMerge) )
import XMonad.Layout.WindowNavigation ( windowNavigation )
import qualified XMonad.Layout.BoringWindows as B
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.Groups.Helpers (swapMaster)

    -- Prompt
import XMonad.Prompt ( Direction1D(Next, Prev) )
import Control.Arrow (first)
import Control.Monad (replicateM_)

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, mkKeymap)
import XMonad.Util.NamedScratchpad ( NamedScratchpad(..)
                                   , customFloating
                                   , defaultFloating
                                   , namedScratchpadAction
                                   , namedScratchpadManageHook
                                   )
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce ( manageSpawn )
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Actions.Submap (submap)
import XMonad.Layout.HintedTile (HintedTile(HintedTile), Alignment (TopLeft), Orientation (Tall, Wide))
import XMonad.Util.XSelection (getSelection)
import XMonad.Actions.FloatKeys (keysResizeWindow, keysMoveWindow)

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"   -- Sets default terminal
-- myTerminal = "kitty"   -- Sets default terminal

myBrowser :: String
myBrowser = "brave"
-- myBrowser = "firefox"
-- myBrowser = "qutebrowser"

myScreenLocker :: String
myScreenLocker = "i3lock-fancy-rapid 10 pixel"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs --fg-daemon'"  -- Sets emacs as editor for tree select
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select

myEmail :: String
myEmail = "emacsclient -c -a emacs --eval '(notmuch)'"

myRofi :: String
myRofi = "rofi -modi drun,ssh,window -show drun -show-icons"

myDmenu :: String
myDmenu = "dmenu_run"

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  spawn "xset r rate 250 80"
  spawn "picom -cf -i 0.8 --use-ewmh-active-win"
  <+> setWMName "LG3D"




--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- The same as above but with the smartBorder
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True


-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Mononoki Nerd Font:Regular:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }


-- The layout hook
myLayoutHook = avoidStruts
             $ mouseResize
               myDefaultLayout
  where
    myDefaultLayout = renamed [Replace "tall"] (mkToggleAll tall)
    mkToggleAll l = windowArrange
                  $ T.toggleLayouts floats
                  $ mkToggle (NBFULL ?? EOT)
                  . mkToggle (single MIRROR)
                  $ l
    tall = mkLayout $ ResizableTall 1 (3/100) (1/2) []
    floats = mkLayout simplestFloat
    mkLayout layout
      = windowNavigation
      $ B.boringWindows
      $ mySpacing 4
      layout



myWorkspaces :: [String]
myWorkspaces = [" 1 ", " 2 ", " 3 "]

clickable :: String -> String
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where
    i = fromJust $ M.lookup ws $ M.fromList $ zip myWorkspaces [1..]


data App
  = ClassApp { name :: String, px :: Rational, py :: Rational, wd :: Rational, ht :: Rational, cmd :: String }
  | TitleApp { name :: String, px :: Rational, py :: Rational, wd :: Rational, ht :: Rational, cmd :: String }
  | NameApp  { name :: String, px :: Rational, py :: Rational, wd :: Rational, ht :: Rational, cmd :: String }
  deriving Show


myManageHook = composeOne $
  [ resource =? t                        -?> doFloat       | t <- byResource ] ++
  [ className =? c                       -?> doFloat       | c <- byClass    ] ++
  [ title =? t                           -?> doFloat       | t <- byTitle    ] ++
  [ stringProperty "WM_WINDOW_ROLE" =? r -?> doFloat       | r <- byRole     ] ++
  [ isInProperty "_NET_WM_WINDOW_TYPE" t -?> doCenterFloat | t <- byType     ] ++
  [ isDialog                             -?> doCenterFloat
  ]
  where
    byResource = ["Devtools", "plasmashell"]
    byClass = [ "Org.gnome.Nautilus" ]
    byTitle = ["Open Document", "Open Files", "Developer Tools"]
    byRole = ["pop-up", "GtkFileChooserDialog", "bubble"]
    byType = ["_NET_WM_WINDOW_TYPE_SPLASH", "_NET_WM_WINDOW_TYPE_DIALOG"]


scratchpads :: [NamedScratchpad]
scratchpads =
  mkNS <$>
  [ TitleApp "emacsSP"       (4/32) (1/32) (24/32) (30/32) "emacsclient -s emacsSP -c -a 'emacs --title emacsSP --bg-daemon=emacsSP'"
  , TitleApp "tmux"          (4/32) (1/32) (24/32) (30/32) (myTerminal ++ " -t tmux -e tmux")
  , TitleApp "htop"          (1/32) (1/32) (30/32) (16/32) (myTerminal ++ " -t htop -e htop")
  , TitleApp "btm"           (16/32) (1/32) (15/32) (30/32) (myTerminal ++ " -t btm -e btm")
  , ClassApp "Brave-browser" (4/32) (1/32) (24/32) (30/32) myBrowser
  ]
  where
    mkNS TitleApp {..} = NS name cmd (title =? name) (customFloating $ W.RationalRect px py wd ht)
    mkNS ClassApp {..} = NS name cmd (className =? name) (customFloating $ W.RationalRect px py wd ht)
    mkNS NameApp {..} = NS name cmd (appName =? name) (customFloating $ W.RationalRect px py wd ht)




main :: IO ()
main = do
    home <- getHomeDirectory
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc"
    xmproc2 <- spawnPipe "xmobar -x 2 $HOME/.config/xmobar/xmobarrc"
    xmonad $ let
      conf = ewmhFullscreen def
        { manageHook = myManageHook <+> namedScratchpadManageHook scratchpads
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = refocusLastLayoutHook $ showWName' myShowWNameTheme myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = 2
        , normalBorderColor  = "#b3afc2"
        , focusedBorderColor = "#ffd700"
        , logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput          = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x
                        , ppCurrent         = xmobarColor "#98be65" "" . wrap "[" "]"                  -- Current workspace in xmobar
                        , ppVisible         = xmobarColor "#98be65" ""               -- . clickable        -- Visible but not current workspace
                        , ppHidden          = xmobarColor "#82AAFF" "" . wrap "*" "" -- . clickable   -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#c792ea" ""               -- . clickable       -- Hidden workspaces (no windows)
                        , ppTitle           = xmobarColor "#b3afc2" "" . shorten 60     -- Title of active window in xmobar
                        , ppSep             =  "<fc=#666666> <fn=1>|</fn> </fc>"          -- Separators in xmobar
                        , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        , ppExtras          = [windowCount]                           -- # of windows current workspace
                        , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }
        }
        in conf `additionalKeysP` myKeys home conf


-- Log Hook:

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

------------------------------------------------------------------------
-- Key Bindings
myKeys home conf =
    -- Xmonad
    [ -- ("M-q"       , spawn "xmonad --recompile; xmonad --restart")
      -- ("M-q"       , spawn "restart-xmonad.sh")
      ("M-C-q"                  , spawn "xmonad-restart")
    , ("M-C-S-q"                , io exitSuccess)         -- Quits xmonad

    -- Launch programs
    , ("M-p"                    , spawn myDmenu)
    , ("M-S-p"                  , spawn myRofi)

    , ("M-s"                    , spawn "dm-search.sh")
    , ("M-v"                    , spawn "clipmenu")
    , ("M-c"                    , spawn "mkdir -p ~/captures; flameshot gui -p ~/captures/")
    -- , ("M-o"                    , spawn "dmenu_run -i -p \"Run: \"")
    , ("M-/"                    , spawn "dm-qutebrowser-history.sh")

    -- Windows navigation
    , ("M-S-m"                  , swapMaster)                -- Moves focused window to master, others maintain order
    , ("M-k"                    , B.focusUp)                 -- Move focus to the prev window, skipiping hidden windows
    , ("M-j"                    , B.focusDown)               -- Move focus to the next window, skipiping hidden windows
    , ("M-m"                    , B.focusMaster)             -- Move focus to the master window, skipiping hidden windows
    , ("M-h"                    , windows W.focusUp)         -- Move focus to the prev window
    , ("M-l"                    , windows W.focusDown)       -- Move focus to the next window
    -- , ("M-h"                    , B.focusUp)                 -- Move focus to the prev window, skipiping hidden windows
    -- , ("M-l"                    , B.focusDown)               -- Move focus to the next window, skipiping hidden windows
    , ("M-C-<Tab>"              , rotAllDown)                -- Rotate all the windows in the current stack
    , ("M-C-S-<Tab>"            , rotSlavesDown)             -- Rotate all windows except master and keep focus in place
    , ("M-n"                    , toggleFocus)               -- Move focus to the lastly focused
    , ("M-S-n"                  , swapWithLast)              -- Move the focused to the lastly focused

    -- boring windows, which are skipped in navigation
    , ("M-S-b"                  , B.markBoring)
    , ("M-M1-b"                 , B.clearBoring)

    -- Kill windows
    , ("M-S-c"                  , kill1)                  -- Kill the currently focused client
    , ("M-S-a"                  , killAll)                -- Kill all windows on current workspace

    -- Workspaces
    , ("M-["                    , moveTo Prev anyWS)                         -- moveTo previous workspace
    , ("M-]"                    , moveTo Next anyWS)                         -- moveTo next workspace
    , ("M-`"                    , toggleWS)
    , ("M-S-["                  , shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws and move
    , ("M-S-]"                  , shiftTo Next nonNSP >> moveTo Next nonNSP)  -- Shifts focused window to next ws and move
    , ("M-C-["                  , prevScreen)                                 -- Switch focus to prev monitor
    , ("M-C-]"                  , nextScreen)                                 -- Switch focus to next monitor
    , ("M-C-S-["                , shiftPrevScreen >> prevScreen)              -- Shifts focused window to prev monitor and move
    , ("M-C-S-]"                , shiftNextScreen >> nextScreen)              -- Shifts focused window to next monitor and move

    -- Floating windows
    , ("M-t"                     , withFocused $ windows . W.sink)  -- Push floating window back to tile
    , ("M-S-t"                   , sinkAll)                         -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
    , ("M--"                     , decWindowSpacing 1)         -- Decrease window spacing
    , ("M-="                     , incWindowSpacing 1)         -- Increase window spacing
    , ("M-S--"                   , decScreenSpacing 1)         -- Decrease screen spacing
    , ("M-S-="                   , incScreenSpacing 1)         -- Increase screen spacing

    -- Layouts
    , ("M-<Space>"               , sendMessage NextLayout)
    , ("M-r"                     , sendMessage $ MT.Toggle MIRROR)

    , ("M-C-M1-<Up>"             , sendMessage Arrange)
    , ("M-C-M1-<Down>"           , sendMessage DeArrange)
    , ("M-f"                     , sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
    , ("M-S-f"                   , sendMessage ToggleStruts)
    , ("M-C-f"                   , sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout


    -- Increase/decrease windows in the master pane or the stack
    , ("M-,"                    , sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
    , ("M-."                    , sendMessage (IncMasterN (-1)))   -- Decrease number of clients in master pane
    -- , ("M-C-,"        , increaseLimit)                   -- Increase number of windows
    -- , ("M-C-."        , decreaseLimit)                   -- Decrease number of windows


    -- Window resizing
    , ("M-C-h"                  , sendMessage Shrink)              -- Shrink horiz window width
    , ("M-C-l"                  , sendMessage Expand)              -- Expand horiz window width
    , ("M-C-j"                  , sendMessage MirrorShrink)        -- Shrink vert window width
    , ("M-C-k"                  , sendMessage MirrorExpand)        -- Exoand vert window width

    , ("M-C-i"                  , withFocused $ keysResizeWindow (0,9) (1/2,1/2))
    , ("M-C-u"                  , withFocused $ keysResizeWindow (0,-9) (1/2,1/2))
    , ("M-C-o"                  , withFocused $ keysResizeWindow (16,0) (1/2,1/2))
    , ("M-C-y"                  , withFocused $ keysResizeWindow (-16,0) (1/2,1/2))

    -- Window moving
    , ("M-i"                    , withFocused $ keysMoveWindow (0,-9))
    , ("M-u"                    , withFocused $ keysMoveWindow (0,9))
    , ("M-o"                    , withFocused $ keysMoveWindow (16,0))
    , ("M-y"                    , withFocused $ keysMoveWindow (-16,0))

    -- emacs in tiling
    , ("M-S-d"                  , spawn myEditor)

    -- Scratchpads
    , ("M-a"                    , namedScratchpadAction scratchpads "tmux")
    , ("M-d"                    , namedScratchpadAction scratchpads "emacsSP")
    , ("M-z"                    , namedScratchpadAction scratchpads "htop")
    , ("M-x"                    , namedScratchpadAction scratchpads "btm")
    , ("M-b"                    , namedScratchpadAction scratchpads "Brave-browser")

    -- environment
    , ("M-M1-9"                 , spawn "xbacklight -inc 5")
    , ("M-M1-8"                 , spawn "xbacklight -dec 5")

    -- Multimedia Keys
    , ("<XF86AudioPlay>"         , spawn (myTerminal ++ " mocp --play"))
    , ("<XF86AudioPrev>"         , spawn (myTerminal ++ " mocp --previous"))
    , ("<XF86AudioNext>"         , spawn (myTerminal ++ " mocp --next"))
    , ("<XF86AudioMute>"         , spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>"  , spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>"  , spawn "amixer set Master 5%+ unmute")
    , ("<XF86MonBrightnessUp>"   , spawn "xbacklight -inc 5")
    , ("<XF86MonBrightnessDown>" , spawn "xbacklight -dec 5")
    , ("<XF86Favorites>"         , spawn myScreenLocker)
    , ("<F12>"                   , spawn myScreenLocker)
    , ("M-M1-C-S-l"              , spawn myScreenLocker)
    , ("<XF86HomePage>"          , spawn myBrowser)
    , ("<XF86Search>"            , safeSpawn myBrowser ["https://hoogle.hackage.org"])
    , ("<XF86Mail>"              , spawn myEmail)
    , ("<XF86Calculator>"        , runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
    , ("<XF86Eject>"             , spawn "toggleeject")
    , ("<Print>"                 , spawn "scrotd 0")
    ]

    -- screen view and shift
    ++ [("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
         | (k,sc) <- zip ["q", "w", "e"] [0..]
         , (f, m) <- [(W.view, ""), (W.shift, "S-")]
       ]

    -- The following lines are needed for named scratchpads.
    where
      nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
      nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
