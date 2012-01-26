import Control.Monad(liftM)

import Data.Bits
import Data.String(fromString)
import qualified Data.Map as M
import qualified Data.Text.Lazy.Encoding as TL(decodeUtf8)

import DBus.Bus(getSessionBus)
import DBus.Client(send_,newClient,runDBus,Client,DBus)
import DBus.Message(Signal(..))
import DBus.Types(toVariant)

import Graphics.Rendering.Pango.Enums(Weight(..))
import Graphics.Rendering.Pango.Markup(markSpan,SpanAttribute(..))
import Graphics.Rendering.Pango.Layout(escapeMarkup)

import XMonad
import XMonad.Actions.UpdatePointer(updatePointer,PointerPosition(TowardsCentre))
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive(isUnfocused,fadeOutLogHook)
import XMonad.Hooks.ManageHelpers(isFullscreen,doFullFloat)
import XMonad.Util.EZConfig


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

main = do
  dbusClient <- newClient =<< getSessionBus
  xmonad $ gnomeConfig
    { modMask = myModMask
    , logHook = myLogHook dbusClient >> logHook gnomeConfig
    }
    `changeModKey` (shiftMask, myShiftMask)
    `changeKeys`
    [ ((myModShiftMask, xK_p), (myModMask,      xK_r)) -- %! Launch gmrun
    , ((myModMask,      xK_q), (myModMask,      xK_x)) -- %! Restart xmonad
    , ((myModShiftMask, xK_q), (myModShiftMask, xK_x)) -- %! Quit xmonad
    , ((myModShiftMask, xK_c), (myModMask,      xK_q)) -- %! Close the focused window
    ]

-- LogHook --
sendUpdateSignal :: String -> DBus ()
sendUpdateSignal output = send_ Signal
    { signalPath = fromString "/org/xmonad/Log"
    , signalMember = fromString "Update"
    , signalInterface = fromString "org.xmonad.Log"
    , signalDestination = Nothing
    , signalBody = [toVariant (TL.decodeUtf8 (fromString output))]
    }

myPrettyPrinter :: Client -> PP
myPrettyPrinter client = defaultPP
    { ppOutput = runDBus client . sendUpdateSignal
    , ppTitle = escapeMarkup
    , ppCurrent = markSpan
        [ FontWeight WeightBold
        , FontForeground "green"
        ] . escapeMarkup
    , ppVisible = const ""
    , ppHidden = escapeMarkup
    , ppLayout = markSpan
        [ FontWeight WeightBold
        , FontForeground "white"
        ] . escapeMarkup
    , ppUrgent = markSpan
        [ FontWeight WeightBold
        , FontForeground "red"
        ]
    }

myLogHook :: Client -> X ()
myLogHook client = do
    dynamicLogWithPP (myPrettyPrinter client)
    updatePointer (TowardsCentre 0.6 0.6)
    fadeOutLogHook fadeRules

fadeRules :: Query Rational
fadeRules = do
    fullscreen <- isFullscreen
    focused <- liftM not isUnfocused
    return $ case () of _ | fullscreen -> 1
                          | focused -> 0.85
                          | otherwise -> 0.8
