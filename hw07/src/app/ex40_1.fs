module ex40_1

open System

type RingBuilder (vals: int) =
  member this.Bind (x, f) = f (x % vals)
    
  member this.Return x = if (x % vals >= 0) then (x % vals) else ( vals + x % vals)

