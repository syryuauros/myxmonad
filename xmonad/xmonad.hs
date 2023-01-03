{-# LANGUAGE FlexibleContexts #-}


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
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS ( moveTo, shiftTo, WSType(..)
                              , nextScreen, prevScreen
                              , shiftPrevScreen, shiftNextScreen
                              , toggleWS
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
myScreenLocker = "i3lock-fancy-rapid 5 pixel"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs --fg-daemon'"  -- Sets emacs as editor for tree select
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select

myEmail :: String
myEmail = "emacsclient -c -a emacs --eval '(notmuch)'"

myEditorOnScratchPad :: String
myEditorOnScratchPad = "emacsclient -s editorSP -c -a 'emacs --title editorSP --fg-daemon=editorSP'"

myRofi :: String
myRofi = "rofi -modi drun,ssh,window -show drun -show-icons"

myDmenu :: String
myDmenu = "dmenu_run"

myTrayer :: String
myTrayer =  "trayer --edge top --align right --widthtype request --padding 1 "
         <> "--SetDockType true --SetPartialStrut true --expand true --transparent true "
         <> "--alpha 0 --tint 0x282c34  --height 20 --distance 0 --distancefrom right &"


altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  spawn "picom -CGcf -i 0.7 -I 1.0 -O 1.0 -D 0 --detect-client-leader"
  <+> setWMName "LG3D"




--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- The same as above but with the smartBorder
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#8EA4B7"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#8EA4B7"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Mononoki Nerd Font:Regular:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }


-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
mySubLayout = windowArrange
            $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
            $ mkToggle (single MIRROR)
            . mkToggle (single REFLECTX)
            . mkToggle (single REFLECTY)
            $ (    Simplest
               ||| subTall
               ||| subMultiCol
              )
  where
    subTall = mySpacing 1
            $ windowNavigation
            $ ResizableTall 1 (3/100) (1/2) []
    subMultiCol = mySpacing 1
                $ windowNavigation
                $ multiCol [1] 0 0.02 0.5

mkLayout layout = windowNavigation
     $ addTabs shrinkText myTabTheme
     $ B.boringWindows
     $ subLayout [] (smartBorders mySubLayout)
     $ mySpacing 4
     layout

tall = mkLayout $ ResizableTall 1 (3/100) (1/2) []
wide = Mirror tall
grid = mkLayout $ Grid (16/10)
myMultiCol = mkLayout $ multiCol [1] 0 0.02 0.5
-- twopane  = renamed [Replace "twopane"]
--          $ windowNavigation
--          $ addTabs shrinkText myTabTheme
--          $ subLayout [] Simplest -- (smartBorders Simplest)
--          $ limitWindows 12
--          $ mySpacing 4
--          $ TwoPane (3/100) (1/2)
magnify  = mkLayout $ ResizableTall 1 (3/100) (1/2) []
monocle  = mkLayout $ limitWindows 20 Full
floats   = mkLayout simplestFloat
spirals  = mkLayout
         $ mySpacing' 2
         $ limitWindows 12
         $ spiral (6/7)
threeCol = mkLayout $ ThreeCol 1 (3/100) (1/2)

accordion = mkLayout Accordion

-- The layout hook
myLayoutHook = avoidStruts
             $ mouseResize
               myDefaultLayout
             where
               myDefaultLayout =   renamed [Replace "tall"] (mkToggleAll tall)
                               ||| renamed [Replace "wide"] (mkToggleAll wide)
                               ||| renamed [Replace "grid"] (mkToggleAll grid)
                               ||| renamed [Replace "multiCol"] (mkToggleAll myMultiCol)
                               ||| renamed [Replace "monocle"] (mkToggleAll (noBorders monocle))
                               ||| renamed [Replace "threeCol"] (mkToggleAll threeCol)
                               ||| renamed [Replace "spirals"] (mkToggleAll spirals)
                               ||| renamed [Replace "magnify"] (mkToggleAll magnify)
                               ||| renamed [Replace "accordion"] (mkToggleAll accordion)
                               --- ||| floats
                               --- ||| threeRow
                               --- ||| noBorders tabs
                               --- ||| twopane
               mkToggleAll l = windowArrange
                             $ T.toggleLayouts floats
                             $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
                             . mkToggle (single MIRROR)
                             . mkToggle (single REFLECTX)
                             . mkToggle (single REFLECTY)
                             $ l
myWorkspaces :: [String]
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]

clickable :: String -> String
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where
    i = fromJust $ M.lookup ws $ M.fromList $ zip myWorkspaces [1..]


data App
  = ClassApp { getName :: String, getCommand :: String}
  | TitleApp { getName :: String, getCommand :: String}
  | NameApp { getName :: String, getCommand :: String}
  deriving Show

audacious  = ClassApp "Audacious"            "audacious"
eog        = NameApp  "eog"                  "eog"
evince     = ClassApp "Evince"               "evince"
gimp       = ClassApp "Gimp"                 "gimp"
nautilus   = ClassApp "Org.gnome.Nautilus"   "nautilus"
pavuctrl   = ClassApp "Pavucontrol"          "pavucontrol"
termSP     = ClassApp "termSP"               (myTerminal ++ " --class termSP")
htopSP     = ClassApp "htopSP"               (myTerminal ++ " --class htopSP -e htop")
btmSP      = ClassApp "btm"                  (myTerminal ++ " --class btmSP")
editorSP   = TitleApp "editorSP"             myEditorOnScratchPad


myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = manageApps <+> manageSpawn <+> manageScratchpads <+> manageDocks
 where
  manageScratchpads = namedScratchpadManageHook myScratchPads


manageApps = composeOne
  [ (className =? "Org.gnome.Nautilus")      -?> doFloat
  , match [ gimp ]                           -?> doCenterFloat
  , match [ audacious
          , eog
          , nautilus
          , pavuctrl
          ]                                  -?> doCenterFloat
  , match [ evince ]                         -?> doFullFloat
  , resource =? "desktop_window"             -?> doIgnore
  , resource =? "kdesktop"                   -?> doIgnore
  , anyOf [ isBrowserDialog
          , isFileChooserDialog
          , isDialog
          , isPopup
          , isSplash
          ]                                  -?> doCenterFloat
  , isFullscreen                             -?> doFullFloat
  , pure True                                -?> tileBelow
  ]
 where
  isBrowserDialog     = isDialog <&&> className =? "Brave-browser"
  isFileChooserDialog = isRole =? "GtkFileChooserDialog"
  isPopup             = isRole =? "pop-up"
  isSplash            = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
  isRole              = stringProperty "WM_WINDOW_ROLE"
  anyOf :: [Query Bool] -> Query Bool
  anyOf = foldl (<||>) (pure False)
  match :: [App] -> Query Bool
  match = anyOf . fmap isInstance
  tileBelow           = insertPosition Below Newer
  doCalendarFloat     = customFloating (W.RationalRect (11 / 15) (1 / 48) (1 / 8) (1 / 8))


isInstance :: App -> Query Bool
isInstance (ClassApp c _) = className =? c
isInstance (TitleApp t _) = title =? t
isInstance (NameApp n _)  = appName =? n


scratchpadApp :: App -> NamedScratchpad
scratchpadApp app = NS (getName app) (getCommand app) (isInstance app) spFloating
  where
    spFloating = customFloating $ W.RationalRect l t w h
                       where
                         h = 0.9
                         w = 0.9
                         t = (1.0 - h)/2
                         l = (1.0 - w)/2


myScratchPads = scratchpadApp <$> [ termSP, htopSP, editorSP, btmSP ]


main :: IO ()
main = do
    home <- getHomeDirectory
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc"
    xmproc2 <- spawnPipe "xmobar -x 2 $HOME/.config/xmobar/xmobarrc"
    xmonad $ let
      conf = ewmhFullscreen def
        { manageHook = myManageHook
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

mySubLayoutKeys :: [(String, X ())]
mySubLayoutKeys =
    [
      ("<Space>"  , toSubl NextLayout)
    , ("`"        , toSubl $ MT.Toggle MIRROR)
    , ("x"        , toSubl $ MT.Toggle REFLECTX)
    , ("y"        , toSubl $ MT.Toggle REFLECTY)
    , (","        , toSubl $ IncMasterN 1)  -- Switch focus to next tab
    , ("."        , toSubl $ IncMasterN (-1))    -- Switch focus to prev tab

    , ("m"        , withFocused (sendMessage . MergeAll))
    , ("u"        , withFocused (sendMessage . UnMerge))
    , ("/"        , withFocused (sendMessage . UnMergeAll))

    , ("u"        , onGroup W.focusDown')  -- Switch focus to next tab
    , ("i"        , onGroup W.focusUp')    -- Switch focus to prev tab
    , ("S-,"      , onGroup W.focusDown')  -- Switch focus to next tab
    , ("S-."      , onGroup W.focusUp')    -- Switch focus to prev tab

    -- Window resizing
    , ("h"        , toSubl Shrink)              -- Shrink horiz window width
    , ("l"        , toSubl Expand)              -- Expand horiz window width
    , ("j"        , toSubl MirrorShrink)        -- Shrink vert window width
    , ("k"        , toSubl MirrorExpand)        -- Exoand vert window width
    , ("M-h"      , toSubl Shrink)              -- Shrink horiz window width
    , ("M-l"      , toSubl Expand)              -- Expand horiz window width
    , ("M-j"      , toSubl MirrorShrink)        -- Shrink vert window width
    , ("M-k"      , toSubl MirrorExpand)        -- Exoand vert window width
    ]

myKeys :: String -> XConfig l -> [(String, X ())]
myKeys home conf =
    -- Xmonad
    [ -- ("M-q"       , spawn "xmonad --recompile; xmonad --restart")
      -- ("M-q"       , spawn "restart-xmonad.sh")
      ("M-C-q"     , spawn "restart-xmonad.sh")
    , ("M-C-S-q"   , io exitSuccess)         -- Quits xmonad

    -- Launch programs
    , ("M-p"     , spawn myDmenu)
    , ("M-S-p"   , spawn myRofi)

    , ("M-a n"   , spawn myEmail)
    , ("M-a w"   , spawn "dm-wifi.sh")
    , ("M-a t"   , spawn myTrayer)
    , ("M-a y"   , spawn "killall trayer")
    , ("M-a l"   , spawn myScreenLocker)
    , ("M-a c"   , spawn "mkdir -p ~/captures; flameshot full -p ~/captures/")
    , ("M-a f"   , spawn "nautilus")

    , ("M-s"     , spawn "dm-search.sh")
    , ("M-b"     , spawn myBrowser)
    , ("M-v"     , spawn "clipmenu")
    , ("M-c"     , spawn "mkdir -p ~/captures; flameshot gui -p ~/captures/")
    , ("M-d"     , spawn myEditor)
    , ("M-o"     , spawn "dmenu_run -i -p \"Run: \"")
    , ("M-/"     , spawn "dm-qutebrowser-history.sh")


    -- , ("M-S-<Return>", shellPrompt myXPConfig) -- Xmonad Shell Prompt
    -- , ("M-S-<Return>", spawn "dmenu_run -i -p \"Run: \"") -- Dmenu
    -- , ("M-S-<Return>", spawn "rofi -show drun -config ~/.config/rofi/themes/dt-dmenu.rasi -display-drun \"Run: \" -drun-display-format \"{name}\"") -- Rofi

    -- Windows navigation
    , ("M-<Return>"   , whenX (swapHybrid True) dwmpromote)                   -- Moves focused window to master, others maintain order
    , ("M-S-m"        , swapMaster)                -- Moves focused window to master, others maintain order
    -- , ("M-m"          , windows W.focusMaster)  -- Move focus to the master window
    , ("M-k"          , B.focusUp)                 -- Move focus to the prev window, skipiping hidden windows
    , ("M-j"          , B.focusDown)               -- Move focus to the next window, skipiping hidden windows
    , ("M-m"          , B.focusMaster)             -- Move focus to the master window, skipiping hidden windows
    , ("M-h"          , windows W.focusUp)         -- Move focus to the prev window
    , ("M-l"          , windows W.focusDown)       -- Move focus to the next window
    -- , ("M-h"          , B.focusUp)                 -- Move focus to the prev window, skipiping hidden windows
    -- , ("M-l"          , B.focusDown)               -- Move focus to the next window, skipiping hidden windows
    , ("M-S-h"        , windows W.swapUp)          -- Swap focused window with prev window
    , ("M-S-j"        , windows W.swapDown)        -- Swap focused window with next window
    , ("M-S-k"        , windows W.swapUp)          -- Swap focused window with prev window
    , ("M-S-l"        , windows W.swapDown)        -- Swap focused window with next window
    , ("M-C-<Tab>"    , rotAllDown)                -- Rotate all the windows in the current stack
    , ("M-C-S-<Tab>"  , rotSlavesDown)             -- Rotate all windows except master and keep focus in place
    , ("M-n"          , toggleFocus)               -- Move focus to the lastly focused
    , ("M-S-n"        , swapWithLast)              -- Move the focused to the lastly focused

    -- boring windows, which are skipped in navigation
    , ("M-S-b"        , B.markBoring)
    , ("M-M1-b"        , B.clearBoring)

    -- Kill windows
    , ("M-S-c"        , kill1)                  -- Kill the currently focused client
    , ("M-S-a"        , killAll)                -- Kill all windows on current workspace

    -- Workspaces
    , ("M-["     , moveTo Prev nonNSP)               -- moveTo previous workspace
    , ("M-]"     , moveTo Next nonNSP)               -- moveTo next workspace
    , ("M-`"     , toggleWS)
    , ("M-S-["   , shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws and move
    , ("M-S-]"   , shiftTo Next nonNSP >> moveTo Next nonNSP)  -- Shifts focused window to next ws and move
    , ("M-C-["   , prevScreen)                       -- Switch focus to prev monitor
    , ("M-C-]"   , nextScreen)                       -- Switch focus to next monitor
    , ("M-C-S-[" , shiftPrevScreen >> prevScreen)    -- Shifts focused window to prev monitor and move
    , ("M-C-S-]" , shiftNextScreen >> nextScreen)    -- Shifts focused window to next monitor and move

    -- Floating windows
    , ("M-t"          , withFocused $ windows . W.sink)  -- Push floating window back to tile
    , ("M-S-t"        , sinkAll)                         -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
    , ("M--"          , decWindowSpacing 1)         -- Decrease window spacing
    , ("M-="          , incWindowSpacing 1)         -- Increase window spacing
    , ("M-S--"        , decScreenSpacing 1)         -- Decrease screen spacing
    , ("M-S-="        , incScreenSpacing 1)         -- Increase screen spacing

    -- Layouts
    , ("M-<Space>"    , sendMessage NextLayout)
    , ("M-C-1"        , sendMessage $ JumpToLayout "tall")
    , ("M-C-2"        , sendMessage $ JumpToLayout "wide")
    , ("M-C-3"        , sendMessage $ JumpToLayout "grid")
    , ("M-C-4"        , sendMessage $ JumpToLayout "multiCol")
    , ("M-C-5"        , sendMessage $ JumpToLayout "monocle")
    , ("M-C-6"        , sendMessage $ JumpToLayout "threeCol")
    , ("M-C-7"        , sendMessage $ JumpToLayout "spirals")
    , ("M-C-8"        , sendMessage $ JumpToLayout "magnify")
    , ("M-C-9"        , sendMessage $ JumpToLayout "accordion")
    , ("M-C-b"        , sendMessage $ MT.Toggle NOBORDERS)
    , ("M-r"          , sendMessage $ MT.Toggle MIRROR)
    , ("M-x"          , sendMessage $ MT.Toggle REFLECTX)
    , ("M-y"          , sendMessage $ MT.Toggle REFLECTY)


    , ("M-C-M1-<Up>"  , sendMessage Arrange)
    , ("M-C-M1-<Down>", sendMessage DeArrange)
    , ("M-f"          , sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
    , ("M-S-f"        , sendMessage ToggleStruts)
    , ("M-C-f"        , sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout


    -- Increase/decrease windows in the master pane or the stack
    , ("M-,"          , sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
    , ("M-."          , sendMessage (IncMasterN (-1)))   -- Decrease number of clients in master pane
    -- , ("M-C-,"        , increaseLimit)                   -- Increase number of windows
    -- , ("M-C-."        , decreaseLimit)                   -- Decrease number of windows


    -- Window resizing
    , ("M-C-h"        , sendMessage Shrink)              -- Shrink horiz window width
    , ("M-C-l"        , sendMessage Expand)              -- Expand horiz window width
    , ("M-C-j"        , sendMessage MirrorShrink)        -- Shrink vert window width
    , ("M-C-k"        , sendMessage MirrorExpand)        -- Exoand vert window width


    -- SubLayouts
    , ("M-C-<Space>"  , toSubl NextLayout)
    , ("M-C-r"        , toSubl $ MT.Toggle MIRROR)
    , ("M-C-x"        , toSubl $ MT.Toggle REFLECTX)
    , ("M-C-y"        , toSubl $ MT.Toggle REFLECTY)
    , ("M-C-,"        , toSubl $ IncMasterN 1)  -- Switch focus to next tab
    , ("M-C-."        , toSubl $ IncMasterN (-1))    -- Switch focus to prev tab

    , ("M-M1-h"      , sendMessage $ pullGroup L)
    , ("M-M1-l"      , sendMessage $ pullGroup R)
    , ("M-M1-k"      , sendMessage $ pullGroup U)
    , ("M-M1-j"      , sendMessage $ pullGroup D)
    , ("M-M1-m"      , withFocused (sendMessage . MergeAll))
    , ("M-M1-u"      , withFocused (sendMessage . UnMerge))
    , ("M-M1-/"      , withFocused (sendMessage . UnMergeAll))

    , ("M-C-S-h"      , toSubl Shrink)              -- Shrink horiz window width
    , ("M-C-S-l"      , toSubl Expand)              -- Expand horiz window width
    , ("M-C-S-j"      , toSubl MirrorShrink)        -- Shrink vert window width
    , ("M-C-S-k"      , toSubl MirrorExpand)        -- Exoand vert window width

    , ("M-u"          , onGroup W.focusDown')  -- Switch focus to next tab
    , ("M-i"          , onGroup W.focusUp')    -- Switch focus to prev tab
    , ("M-S-,"        , onGroup W.focusDown')  -- Switch focus to next tab
    , ("M-S-."        , onGroup W.focusUp')    -- Switch focus to prev tab
    , ("M-g"          , submap $ conf  `mkKeymap` mySubLayoutKeys)


    -- Scratchpads
    , ("M-C-<Return>" , namedScratchpadAction myScratchPads "termSP")
    , ("M-C-d"        , namedScratchpadAction myScratchPads "editorSP")
    , ("M-C-b"        , namedScratchpadAction myScratchPads "btmSP")
    , ("M-C-t"        , namedScratchpadAction myScratchPads "htopSP")


    , ("M-M1-9" , spawn "xbacklight -inc 5")
    , ("M-M1-8" , spawn "xbacklight -dec 5")

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
