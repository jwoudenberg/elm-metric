module Metric exposing (..)


type Number unit
    = Number Float unit



-- Unit Combiners


type Mul a b
    = Mul a b


type Div a b
    = Div a b



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

type alias Constructor unit = Float -> Number unit

number : unit -> Constructor unit
number unit value =
    Number value unit


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


kilo : Prefix unit
kilo =
    prefix 1000


milli : Prefix unit
milli =
    prefix 0.001



-- Number operations


add : Number a -> Number a -> Number a
add (Number valueA unit) (Number valueB _) =
    Number (valueA + valueB) unit


sub : Number a -> Number a -> Number a
sub (Number valueA unit) (Number valueB _) =
    Number (valueA - valueB) unit


mul : Number a -> Number b -> Number (Mul a b)
mul (Number valueA unitA) (Number valueB unitB) =
    Number (valueA * valueB) (Mul unitA unitB)


div : Number a -> Number b -> Number (Div a b)
div (Number valueA unitA) (Number valueB unitB) =
    Number (valueA / valueB) (Div unitA unitB)


smul : Float -> Number unit -> Number unit
smul scalar (Number value unit) =
    Number (scalar * value) unit



-- Convenience infix operators


(.+) : Number a -> Number a -> Number a
(.+) =
    add


(.-) : Number a -> Number a -> Number a
(.-) =
    sub


(.*) : Number a -> Number b -> Number (Mul a b)
(.*) =
    mul

(./) : Number a -> Number b -> Number (Div a b)
(./) =
    div

(..) : Float -> Number unit -> Number unit
(..) =
    smul


-- Example use


speed : Number Speed
speed =
    kilo meter 4 ./ second 2


energy : Number Joule
energy =
    milli newton 10 .* meter 4
