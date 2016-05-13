module RandSelect where

import Keyframes as K 
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

randInt : Int -> Int -> Int -> Int 
randInt len upper lower = 
    let seed = Random.initialSeed len in
    let gen = Random.generate (Random.int upper lower) seed in 
    fst gen


nPos : (Float, Float) -> Int -> (Float,Float)
nPos (w,h) n = 
    if n == 4 then ((w/2),-(w/4))
    else if n == 8 then ((w/2), w/20)
    else if n == 1 then (-(w/2), (w/4))
    else if n == 5 then (-(w/2), -(w/6))
    else if n == 9 then (-(w/2), 0)
    else if n == 2 then ((h/2), -(h/4))
    else if n == 6 then ((h/2), (h/7))
    else if n == 10 then ((h/2), 0)
    else (-(h/2),(h/4))

colorGen : Int -> Color.Color
colorGen num = 
    if num == 1 || num == 10 then Color.rgb 102 0 0
    else if num == 2 || num == 9 then Color.rgb 156 15 15 
    else if num == 3 || num == 8 then Color.rgb 153 0 0
    else if num == 4 || num == 7 then Color.rgb 247 47 47 
    else  Color.rgb 204 51 51 

randStart: Int -> Float -> (Float, Float)
randStart rand hfloat = 
    if (rand >= 1)&&(rand<5) then (0,-hfloat/2)
    else (0, hfloat/2)

getDir : Int -> Float -> (Float, Float)
getDir num f = 
    if num == 1 then (0,f)
    else if num == 2 then (f, 0)
    else if num == 3 then (0, -f)
    else if num == 4 then (f, f)
    else if num == 5 then (f, -f)
    else if num == 6 then (-f, -f)
    else if num == 7 then (-f, f)
    else (-f, 0)

backCol : Int -> Color.Color
backCol x = 
    if x == 1 then Color.rgba 87 242 201 0.25
    else if x == 2 then Color.rgba 255 153 204 0.5
    else if x == 3 then Color.rgba 169 252 194 0.4
    else if x == 4 then Color.rgba 154 171 255 0.5
    else Color.rgba 252 219 169 0.5