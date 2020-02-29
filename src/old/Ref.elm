module Ref exposing(..)

type Ref a =
  Ref a

type Ref2 tag value =
  Ref2 value

target : Ref a -> a
target (Ref t) =
  t