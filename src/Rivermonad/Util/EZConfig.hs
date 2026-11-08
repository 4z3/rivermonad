-- | Emacs-style key binding strings, as in @XMonad.Util.EZConfig@.
--
-- > main = rivermonad $ def { ... } `additionalKeysP`
-- >   [ ("M-S-<Return>", spawn "foot")
-- >   , ("M-p",          spawn "fuzzel")
-- >   , ("<XF86AudioMute>", spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
-- >   ]
--
-- Modifiers: @M-@ (the configured 'modMask'), @C-@ (control), @S-@ (shift),
-- @M1-@ … @M5-@ (mod1 … mod5). Keys: a single character (@a@, @1@, @,@) or
-- an xkbcommon keysym name in angle brackets (@\<Return\>@, @\<F5\>@,
-- @\<XF86AudioMute\>@; also the xmonad aliases @\<Esc\>@, @\<Space\>@).
-- Key sequences (@"M-x a"@) are not supported.
module Rivermonad.Util.EZConfig
  ( additionalKeys
  , additionalKeysP
  , removeKeys
  , removeKeysP
  , additionalMouseBindings
  , mkKeymap
  , readKey
  , checkKeymap
  ) where

import Data.Bits ((.|.))
import Data.Char (ord, toLower)
import Data.List (stripPrefix)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import System.IO (hPutStrLn, stderr)

import Rivermonad.Core
import Rivermonad.Keysym

infixl 4 `additionalKeys`, `additionalKeysP`, `removeKeys`, `removeKeysP`, `additionalMouseBindings`

additionalKeys :: XConfig -> [((KeyMask, KeySym), X ())] -> XConfig
additionalKeys conf ks = conf { keys = \cc -> M.union (M.fromList ks) (keys conf cc) }

additionalKeysP :: XConfig -> [(String, X ())] -> XConfig
additionalKeysP conf ks = conf { keys = \cc -> M.union (mkKeymap cc ks) (keys conf cc) }

removeKeys :: XConfig -> [(KeyMask, KeySym)] -> XConfig
removeKeys conf ks = conf { keys = \cc -> foldr M.delete (keys conf cc) ks }

removeKeysP :: XConfig -> [String] -> XConfig
removeKeysP conf ks = conf { keys = \cc -> foldr M.delete (keys conf cc) (mapMaybe (readKey cc) ks) }

additionalMouseBindings :: XConfig -> [((KeyMask, Button), Window -> X ())] -> XConfig
additionalMouseBindings conf ms = conf { mouseBindings = \cc -> M.union (M.fromList ms) (mouseBindings conf cc) }

-- | Build a key map; unparsable strings are skipped (see 'checkKeymap').
mkKeymap :: XConfig -> [(String, X ())] -> M.Map (KeyMask, KeySym) (X ())
mkKeymap cc ks = M.fromList [ (k, a) | (s, a) <- ks, Just k <- [readKey cc s] ]

-- | Report unparsable or duplicated bindings on stderr. Call it from the
-- startup hook: @startupHook = checkKeymap myConf myKeys@.
checkKeymap :: XConfig -> [(String, a)] -> X ()
checkKeymap cc ks = do
  let bad = [ s | (s, _) <- ks, Nothing <- [readKey cc s] ]
      parsed = mapMaybe (readKey cc . fst) ks
      dups = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [ (k, 1) | k <- parsed ]))
  mapM_ (\s -> io (hPutStrLn stderr ("rivermonad: cannot parse key binding " ++ show s))) bad
  mapM_ (\k -> io (hPutStrLn stderr ("rivermonad: duplicated key binding " ++ show k))) dups

-- | Parse one binding such as @"M-S-<Return>"@.
readKey :: XConfig -> String -> Maybe (KeyMask, KeySym)
readKey cc = go 0
  where
    go m s = case modifier s of
      Just (m', rest) -> go (m .|. m') rest
      Nothing -> (,) m <$> key s
    modifier s = firstJust
      [ (\r -> (msk, r)) <$> stripPrefix p s
      | (p, msk) <- [ ("M1-", mod1Mask), ("M2-", 16), ("M3-", mod3Mask), ("M4-", mod4Mask)
                    , ("M5-", mod5Mask), ("M-", modMask cc), ("C-", controlMask), ("S-", shiftMask) ] ]
    key [c] = Just (fromIntegral (ord (toLower c)))
    key ('<' : rest) | not (null rest), last rest == '>' = named (init rest)
    key _ = Nothing
    named n = lookup (map toLower (alias n)) [ (map toLower k, v) | (k, v) <- keysymNames ]
    alias n = case map toLower n of
      "esc" -> "Escape"
      "pageup" -> "Page_Up"
      "pagedown" -> "Page_Down"
      "prior" -> "Page_Up"
      "next" -> "Page_Down"
      "backspace" -> "BackSpace"
      "enter" -> "Return"
      _ -> n
    firstJust xs = case [ x | Just x <- xs ] of
      (x : _) -> Just x
      [] -> Nothing
