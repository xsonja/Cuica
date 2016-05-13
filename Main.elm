module Main where

import Keyframes as K 

import FormMake exposing (..)
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
import RandSelect exposing (..)
import Draw exposing (..)

type alias State = (Int, List (Float, (Char, Int)))
-- int represents the background color 
-- char indicates which animation is currently being played 
-- float [0,1] indicates how far into the animation 

type Event = NewTime Time | NewKey (Char, Int)
-- update the state when a new key is pressed or time elapses

type alias JS = {code:String, seed:Float}
-- the string indicates which key was pressed
-- the float is a random seed based off of the current time

port randomSeed : Signal JS
-- receives a signal from Audio.js of the type described above

-- SENDS SIGNAL FROM KEYS TO AUDIO.JS 
-- CAUSES SOUND TO BE PLAYED
port sendKey : Signal String
port sendKey =      
    Signal.map String.fromChar (Signal.map Char.fromCode Keyboard.presses)



-- PROGRESSES ALL ANIMATIONS 
-- REMOVES COMPLETED ANIMATIONS FROM THE STATE
prune: List (Float, (Char,Int)) -> List (Float, (Char,Int))
prune s =
    case s of 
        [] -> []
        (frame, (id, num))::rest -> 
            if frame > 1.0 then prune rest 
            else 
                if id == 'a' then (frame+0.02, (id, num))::(prune rest)
                else if id == 's' then (frame+0.04, (id, num))::(prune rest)
                else if id == 'd' then (frame+0.02, (id, num))::(prune rest)
                else if id == 'h' then (frame+0.015, (id, num))::(prune rest)
                else if id == 'g' then (frame+0.04, (id, num))::(prune rest)
                else if id == 'y' then (frame+0.02, (id, num))::(prune rest)
                else if id == 'w' then (frame+0.015, (id, num))::(prune rest)
                else if id == 'e' then (frame+0.011, (id, num))::(prune rest)
                else if id == 'x' || id == 'u' then (frame+0.9, (id, num))::(prune rest)
                else if id == 'q' then (frame +0.07, (id, num))::(prune rest)
                else if id == 'i' then (frame +0.004, (id, num))::(prune rest)
                else if id == 'p' then (frame +0.02, (id, num))::(prune rest)
                else if id == 'n' then (frame +0.015, (id, num))::(prune rest)
                else (frame+0.01, (id, num))::(prune rest)

-- function that rotates between the different background colors
changeBkgd : Int -> Int 
changeBkgd x = 
    if x < 5 then x + 1
    else 1

-- converts the strings received through the port into chars
toChar : String -> Char
toChar str = 
    if str == "a" then 'a'
    else if str == "b" then 'b'
    else if str == "c" then 'c'
    else if str == "d" then 'd'
    else if str == "e" then 'e'
    else if str == "f" then 'f'
    else if str == "g" then 'g'
    else if str == "h" then 'h'
    else if str == "i" then 'i'
    else if str == "j" then 'j'
    else if str == "k" then 'k'
    else if str == "l" then 'l'
    else if str == "m" then 'm'
    else if str == "n" then 'n'
    else if str == "o" then 'o'
    else if str == "p" then 'p'
    else if str == "q" then 'q'
    else if str == "r" then 'r'
    else if str == "s" then 's'
    else if str == "t" then 't'
    else if str == "u" then 'u'
    else if str == "v" then 'v'
    else if str == "w" then 'w'
    else if str == "x" then 'x'
    else if str == "y" then 'y'
    else if str == "z" then 'z'
    else if str == " " then ' '
    else '.'

-- IF EVENT IS A KEY, ADDS IT TO STATE
-- IF EVENT IS TIME, PRUNES THE STATE
upstate : Event -> State -> State
upstate e s = 
    let bkgd = fst s in
    let list = snd s in
    case list of 
        [] -> 
            case e of 
                NewKey (code, seed) -> 
                    if code == ' ' then 
                        (changeBkgd bkgd, [])
                    else let rand = ((randInt seed 0 100) % 10) + 1 in
                    (bkgd, [(0,(code,rand))])
                _ -> (bkgd, [])
        (_,(x, y))::rest -> 
            case e of 
                NewTime t -> (bkgd, prune list)
                NewKey (code, seed) -> 
                    if code == ' ' then 
                        (changeBkgd bkgd, list)
                    else 
                        let old = Char.toCode x in 
                        let len = List.length list in
                        let rand = ((randInt seed 0 100) % 10) + 1 in 
                        (bkgd, (0,(code, rand))::list)

jsToEvent : JS -> (Char, Int) 
jsToEvent js = 
    (toChar js.code, round js.seed)

state : Signal State 
state = Signal.foldp upstate (1, []) 
    (Signal.merge 
        (Signal.map NewTime (Time.fps 100))
        (Signal.map NewKey (Signal.map jsToEvent randomSeed)))



view : (Int,Int) -> State -> E.Element
view (w,h) s =
    let bkgd = fst s in 
    let color = backCol bkgd in
    let currs = snd s in
    E.color color (E.container w h E.middle 
    (C.collage w h (List.map (draw (w,h)) currs)))

-- view for testing that prints the state
view2 : (Int,Int) -> State -> E.Element 
view2 (w,h) s = 
    E.show s

main : Signal E.Element
main =
  Signal.map2 view Window.dimensions state


