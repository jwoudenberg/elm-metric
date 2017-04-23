module Metric exposing (..)


type Number unit
    = Number Float



-- Unit Combiners


type Mul a b
    = Mul


type Div a b
    = Div



-- Units


type Newton
    = Newton


type Meter
    = Meter


type Second
    = Second


type alias Joule =
    Mul Newton Meter


type alias Speed =
    Div Meter Second



-- Helpers


type alias Constructor unit =
    Float -> Number unit


number : unit -> Constructor unit
number unit value =
    Number value


type alias Prefix unit =
    Constructor unit -> Constructor unit


prefix : Float -> Prefix unit
prefix multiplier constructor value =
    smul multiplier (constructor value)



-- Nunber constructors


meter : Constructor Meter
meter =
    number Meter


second : Constructor Second
second =
    number Second


newton : Constructor Newton
newton =
    number Newton



-- Prefixes


yocto : Prefix unit
yocto =
    prefix 1.0e-24


zepto : Prefix unit
zepto =
    prefix 1.0e-21


atto : Prefix unit
atto =
    prefix 1.0e-18


femto : Prefix unit
femto =
    prefix 1.0e-15


pico : Prefix unit
pico =
    prefix 1.0e-12


nano : Prefix unit
nano =
    prefix 1.0e-9


micro : Prefix unit
micro =
    prefix 1.0e-6


milli : Prefix unit
milli =
    prefix 1.0e-3


centi : Prefix unit
centi =
    prefix 1.0e-2


deci : Prefix unit
deci =
    prefix 1.0e-1


deka : Prefix unit
deka =
    prefix 1.0e1


hecto : Prefix unit
hecto =
    prefix 1.0e2


kilo : Prefix unit
kilo =
    prefix 1.0e3


mega : Prefix unit
mega =
    prefix 1.0e6


giga : Prefix unit
giga =
    prefix 1.0e9


tera : Prefix unit
tera =
    prefix 1.0e12


peta : Prefix unit
peta =
    prefix 1.0e15


exa : Prefix unit
exa =
    prefix 1.0e18


zetta : Prefix unit
zetta =
    prefix 1.0e21


yotta : Prefix unit
yotta =
    prefix 1.0e24



-- Number operations


add : Number a -> Number a -> Number a
add (Number valueA) (Number valueB) =
    Number (valueA + valueB)


sub : Number a -> Number a -> Number a
sub (Number valueA) (Number valueB) =
    Number (valueA - valueB)


mul : Number a -> Number b -> Number (Mul a b)
mul (Number valueA) (Number valueB) =
    Number (valueA * valueB)


div : Number a -> Number b -> Number (Div a b)
div (Number valueA) (Number valueB) =
    Number (valueA / valueB)


smul : Float -> Number unit -> Number unit
smul scalar (Number value) =
    Number (scalar * value)



-- Convenience infix operators


(.+) : Number a -> Number a -> Number a
(.+) =
    add


(.-) : Number a -> Number a -> Number a
(.-) =
    sub


(.**) : Number a -> Number b -> Number (Mul a b)
(.**) =
    mul


(./) : Number a -> Number b -> Number (Div a b)
(./) =
    div


(.*) : Float -> Number unit -> Number unit
(.*) =
    smul



-- Example use


speed : Number Speed
speed =
    kilo meter 4 ./ second 2


time : Number Second
time =
    second 10


energy : Number Joule
energy =
    milli newton 10 .** meter 4


{-| This doesn't work
-}
distance : Number (Mul (Div Meter Second) Second)
distance =
    speed .** time
