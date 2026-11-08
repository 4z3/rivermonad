-- | xkbcommon keysyms (same values as X11 keysyms) and modifier masks.
-- Use the unshifted keysym together with 'shiftMask', as in xmonad.
module Rivermonad.Keysym where

import Data.Word (Word32)

type KeySym = Word32
type KeyMask = Word32
type Button = Word32

-- | Modifier masks, as understood by river_seat_v1.modifiers (same bits as X11).
noModMask, shiftMask, controlMask, mod1Mask, mod3Mask, mod4Mask, mod5Mask :: KeyMask
noModMask = 0
shiftMask = 1
controlMask = 4
mod1Mask = 8
mod3Mask = 32
mod4Mask = 64
mod5Mask = 128

-- | Pointer buttons as Linux input event codes (BTN_LEFT, BTN_MIDDLE, BTN_RIGHT...).
button1, button2, button3, button4, button5 :: Button
button1 = 0x110 -- BTN_LEFT
button2 = 0x112 -- BTN_MIDDLE
button3 = 0x111 -- BTN_RIGHT
button4 = 0x113 -- BTN_SIDE
button5 = 0x114 -- BTN_EXTRA

xK_Return :: KeySym
xK_Return = 0xff0d
xK_space :: KeySym
xK_space = 0x20
xK_Tab :: KeySym
xK_Tab = 0xff09
xK_Escape :: KeySym
xK_Escape = 0xff1b
xK_BackSpace :: KeySym
xK_BackSpace = 0xff08
xK_Delete :: KeySym
xK_Delete = 0xffff
xK_Home :: KeySym
xK_Home = 0xff50
xK_Left :: KeySym
xK_Left = 0xff51
xK_Up :: KeySym
xK_Up = 0xff52
xK_Right :: KeySym
xK_Right = 0xff53
xK_Down :: KeySym
xK_Down = 0xff54
xK_Page_Up :: KeySym
xK_Page_Up = 0xff55
xK_Page_Down :: KeySym
xK_Page_Down = 0xff56
xK_End :: KeySym
xK_End = 0xff57
xK_Print :: KeySym
xK_Print = 0xff61
xK_Insert :: KeySym
xK_Insert = 0xff63
xK_comma :: KeySym
xK_comma = 0x2c
xK_period :: KeySym
xK_period = 0x2e
xK_slash :: KeySym
xK_slash = 0x2f
xK_minus :: KeySym
xK_minus = 0x2d
xK_equal :: KeySym
xK_equal = 0x3d
xK_plus :: KeySym
xK_plus = 0x2b
xK_semicolon :: KeySym
xK_semicolon = 0x3b
xK_apostrophe :: KeySym
xK_apostrophe = 0x27
xK_grave :: KeySym
xK_grave = 0x60
xK_backslash :: KeySym
xK_backslash = 0x5c
xK_bracketleft :: KeySym
xK_bracketleft = 0x5b
xK_bracketright :: KeySym
xK_bracketright = 0x5d
xK_question :: KeySym
xK_question = 0x3f
xK_a :: KeySym
xK_a = 0x61
xK_b :: KeySym
xK_b = 0x62
xK_c :: KeySym
xK_c = 0x63
xK_d :: KeySym
xK_d = 0x64
xK_e :: KeySym
xK_e = 0x65
xK_f :: KeySym
xK_f = 0x66
xK_g :: KeySym
xK_g = 0x67
xK_h :: KeySym
xK_h = 0x68
xK_i :: KeySym
xK_i = 0x69
xK_j :: KeySym
xK_j = 0x6a
xK_k :: KeySym
xK_k = 0x6b
xK_l :: KeySym
xK_l = 0x6c
xK_m :: KeySym
xK_m = 0x6d
xK_n :: KeySym
xK_n = 0x6e
xK_o :: KeySym
xK_o = 0x6f
xK_p :: KeySym
xK_p = 0x70
xK_q :: KeySym
xK_q = 0x71
xK_r :: KeySym
xK_r = 0x72
xK_s :: KeySym
xK_s = 0x73
xK_t :: KeySym
xK_t = 0x74
xK_u :: KeySym
xK_u = 0x75
xK_v :: KeySym
xK_v = 0x76
xK_w :: KeySym
xK_w = 0x77
xK_x :: KeySym
xK_x = 0x78
xK_y :: KeySym
xK_y = 0x79
xK_z :: KeySym
xK_z = 0x7a
xK_0 :: KeySym
xK_0 = 0x30
xK_1 :: KeySym
xK_1 = 0x31
xK_2 :: KeySym
xK_2 = 0x32
xK_3 :: KeySym
xK_3 = 0x33
xK_4 :: KeySym
xK_4 = 0x34
xK_5 :: KeySym
xK_5 = 0x35
xK_6 :: KeySym
xK_6 = 0x36
xK_7 :: KeySym
xK_7 = 0x37
xK_8 :: KeySym
xK_8 = 0x38
xK_9 :: KeySym
xK_9 = 0x39
xK_F1 :: KeySym
xK_F1 = 0xffbe
xK_F2 :: KeySym
xK_F2 = 0xffbf
xK_F3 :: KeySym
xK_F3 = 0xffc0
xK_F4 :: KeySym
xK_F4 = 0xffc1
xK_F5 :: KeySym
xK_F5 = 0xffc2
xK_F6 :: KeySym
xK_F6 = 0xffc3
xK_F7 :: KeySym
xK_F7 = 0xffc4
xK_F8 :: KeySym
xK_F8 = 0xffc5
xK_F9 :: KeySym
xK_F9 = 0xffc6
xK_F10 :: KeySym
xK_F10 = 0xffc7
xK_F11 :: KeySym
xK_F11 = 0xffc8
xK_F12 :: KeySym
xK_F12 = 0xffc9
xK_XF86AudioMute :: KeySym
xK_XF86AudioMute = 0x1008ff12
xK_XF86AudioLowerVolume :: KeySym
xK_XF86AudioLowerVolume = 0x1008ff11
xK_XF86AudioRaiseVolume :: KeySym
xK_XF86AudioRaiseVolume = 0x1008ff13
xK_XF86AudioPlay :: KeySym
xK_XF86AudioPlay = 0x1008ff14
xK_XF86AudioNext :: KeySym
xK_XF86AudioNext = 0x1008ff17
xK_XF86AudioPrev :: KeySym
xK_XF86AudioPrev = 0x1008ff16
xK_XF86MonBrightnessUp :: KeySym
xK_XF86MonBrightnessUp = 0x1008ff02
xK_XF86MonBrightnessDown :: KeySym
xK_XF86MonBrightnessDown = 0x1008ff03
