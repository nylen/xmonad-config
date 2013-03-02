import qualified Codec.Binary.UTF8.String as UTF8

import Control.Monad(liftM)

import Data.Bits
import Data.String(fromString)
import qualified Data.Map as M

import qualified DBus as D
import qualified DBus.Client as D

import Graphics.Rendering.Pango.Enums(Weight(..))
import Graphics.Rendering.Pango.Markup(markSpan,SpanAttribute(..))
import Graphics.Rendering.Pango.Layout(escapeMarkup)

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive(isUnfocused,fadeOutLogHook)
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig

import CustomGrid

import MateConfig


changeBits :: (Bits a) => a -> a -> a -> a
changeBits val a b
  | val .&. a /= 0 = val .&. complement a .|. b
  | otherwise      = val

changeModKey :: XConfig a -> (KeyMask, KeyMask) -> XConfig a
changeModKey conf (modOld, modNew) =
  conf { keys = \cnf -> M.mapKeys (\(a, b) -> (changeBits a modOld modNew, b)) $ keys conf cnf }

changeKeys :: XConfig a -> [((ButtonMask, KeySym), (ButtonMask, KeySym))] -> XConfig a
changeKeys conf keySpec =
  conf { keys = \cnf -> M.mapKeys (\x -> M.findWithDefault x x keySpecMap) $ keys conf cnf }
  where keySpecMap = M.fromList keySpec


myModMask      = mod4Mask
myShiftMask    = mod1Mask
myModShiftMask = myModMask .|. myShiftMask

myLayout = desktopLayoutModifiers $ Full ||| CustomGrid

main = do
  dbus <- D.connectSession
  getWellKnownName dbus
  xmonad $ mateConfig
    { modMask = myModMask
    , logHook = dynamicLogWithPP (prettyPrinter dbus)
    , layoutHook = myLayout
    -- http://www.haskell.org/haskellwiki/Xmonad/Frequently_asked_questions#Problems_with_Java_applications.2C_Applet_java_console
    , startupHook = startupHook mateConfig >> setWMName "LG3D"
    }
    `changeModKey` (shiftMask, myShiftMask)
    `changeKeys`
    [ ((myModShiftMask, xK_p), (myModMask,      xK_r)) -- %! Launch gmrun
    , ((myModMask,      xK_q), (myModMask,      xK_x)) -- %! Restart xmonad
    , ((myModShiftMask, xK_q), (myModShiftMask, xK_x)) -- %! Quit xmonad
    , ((myModShiftMask, xK_c), (myModMask,      xK_q)) -- %! Close the focused window
    ]

-- Log hook (for xmonad-log-applet) --

-- Colors from /etc/X11/rgb.txt
-- (http://developer.gnome.org/pygtk/2.22/class-pangocolor.html)
prettyPrinter :: D.Client -> PP
prettyPrinter client = defaultPP
    { ppOutput = dbusOutput client
    , ppTitle = escapeMarkup
    , ppCurrent = markSpan
        [ FontWeight WeightBold
        , FontForeground "sea green"
        ] . escapeMarkup
    , ppVisible = const ""
    , ppHidden = escapeMarkup
    , ppLayout = markSpan
        [ FontWeight WeightBold
        , FontForeground "gray50"
        ] . escapeMarkup
    , ppUrgent = markSpan
        [ FontWeight WeightBold
        , FontForeground "red"
        ]
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName client = do
    D.requestName client (D.busName_ "org.xmonad.Log")
                  [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput client str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
        D.signalBody = [D.toVariant (UTF8.decodeString str)]
    }
    D.emit client signal
