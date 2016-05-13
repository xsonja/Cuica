module Animations where

import Keyframes as K 
import Keyframes.Easing as Ease
import Window
import Color
import Text as T
import Graphics.Element as E
import Graphics.Collage as C exposing (defaultLine)
import Graphics.Input exposing (button, customButton)
import Time exposing (Time)
import Signal exposing (Mailbox, mailbox)
import Keyboard
import Char
import String
import Random
import FormMake exposing (..)
import RandSelect exposing (..)

-- FUNCTIONS FOR ANIMATIONS USING KEYFRAMES
-- FUNCTIONS FOR DRAWING USING COLLAGE
-- (to be moved to another file)
expand : Float -> String -> String -> Float -> K.State 
expand totalTime start end frame = 
    K.playOnce totalTime (K.transition "width" start end) frame

quadExpand : Float -> String -> String -> Float -> K.State 
quadExpand totalTime start end frame = 
    K.playOnce totalTime (Ease.easeInQuad (K.transition "width" start end)) frame

quadInOutExpand : Float -> String -> String -> Float -> K.State 
quadInOutExpand totalTime start end frame = 
    K.playOnce totalTime (Ease.easeInOutQuad (K.transition "width" start end)) frame

expExpand : Float -> String -> String -> Float -> K.State 
expExpand totalTime start end frame = 
    K.playOnce totalTime (Ease.easeInExpo (K.transition "width" start end)) frame

expandShrink : Float -> String -> String -> Float -> K.State 
expandShrink totalTime start end frame = 
    let 
        ex = K.transition "width" start end
        exs = K.toAndFrom ex 
    in
    K.playOnce totalTime exs frame

quadInOutexpandShrink : Float -> String -> String -> Float -> K.State 
quadInOutexpandShrink totalTime start end frame = 
    let 
        ex = K.transition "width" start end
        exs = K.toAndFrom ex 
    in
    K.playOnce totalTime (Ease.easeInOutQuad exs) frame

shrinkRotate : Float -> String -> String -> String-> String-> Float -> K.State
shrinkRotate totalTime startang endang startsize endsize frame = 
    let rotated = K.reverse (K.transition "rotate" startang endang) in
    let shrinkmerge = K.merge [rotated, (K.transition "width" startsize endsize)] in
    K.playOnce totalTime shrinkmerge frame

expandwait : Float -> String -> String -> Float -> K.State 
expandwait totalTime start end frame = 
    let expand = K.transition "width" start end in
    K.playOnce totalTime (K.linger 0.5 expand) frame   

quadOutexpandwait : Float -> String -> String -> Float -> K.State 
quadOutexpandwait totalTime start end frame = 
    let expand = K.transition "width" start end in
    K.playOnce totalTime (Ease.easeOutCubic (K.linger 0.5 expand)) frame   

fade : Float -> String -> String -> Float -> K.State  
fade totalTime start end frame = 
    K.playOnce totalTime (K.transition "opacity" start end) frame

fadeExpand : Float -> String -> String -> Float -> K.State  
fadeExpand totalTime start end frame =
    let --opacity = Ease.easeInExpo (K.reverse (K.transition "opacity" start end))
        opacity = Ease.easeInExpo (K.transition "opacity" start end)
        exp = (Ease.easeInQuad (K.transition "width" start end))
        merged = K.merge [opacity, exp]
    in
    K.playOnce totalTime merged frame

angle : Float -> String -> String -> Float -> K.State
angle totalTime start end frame =
    K.playOnce totalTime (K.transition "rotate" start end) frame

growShrink : Float -> String -> String -> String -> Float -> K.State
growShrink totalTime start end negend frame = 
    let grow = K.transition "width" start end in
    let both = K.toAndFrom grow in
    let grow2 = K.transition "width" start negend in 
    let both2 = K.toAndFrom grow2 in
    let combo = K.chain [(0, both), (0.5, both2)] in
    K.playOnce totalTime combo frame

moveShrink : Float -> String -> String -> String -> String -> Float -> K.State
moveShrink totalTime startpos endpos startsize endsize frame =
    let move = (Ease.easeOutQuad(K.transition "position" startpos endpos)) in
    let movemerge = K.merge [move, (K.linger 0.5 (K.transition "width" startsize startsize))] in
    let shrink = (Ease.easeInExpo (K.transition "width" startsize endsize)) in
    let shrinkmerge = K.merge [shrink, (K.linger 0.5 (K.transition "position" endpos endpos))] in
    let both = K.chain [(0,movemerge), (0.5, shrinkmerge)] in
    K.playOnce totalTime both frame


------------------------------------------
------------------------------------------


getnFrames : Int -> (Int,Int) -> Float -> K.State
getnFrames n (w,h) frame = 
    if n == 0 then []
    else 
        let 
            curr = growShrink 1.0 "2.0" (toString (w//5)) (toString -(w//5)) frame
        in 
        List.append curr (getnFrames (n-1) (w,h) (frame+0.04))
