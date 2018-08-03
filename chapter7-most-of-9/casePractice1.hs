-- functionC x y = if (x > y) then x else y

functionC x y =
  case (x > y) of
    True -> x
    False -> y
