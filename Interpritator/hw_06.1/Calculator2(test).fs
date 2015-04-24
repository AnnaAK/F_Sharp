module Calculator2
open toTree
open Stmt

(*type Expr = Num of int
            | Var of string
            | Op of string * Expr * Expr*)

let calculate (expr: Tree) (context: ValofVar<'A, 'B>) =
  let rec calc tree =
        match tree with
        | Num (value) -> value
        | Var (var) -> context.find(var)
        | Op (v, tree1, tree2) -> let a = calc tree1
                                  let b = calc tree2
                                  match v with
                                  | "+" -> a + b
                                  | "-" -> a - b
                                  | "*" -> a * b
                                  | "/" -> a / b
                                  | "%" -> b % a 
                                  | "^" ->
                                          let rec pow e s =
                                            match s with
                                            | 0 -> 1
                                            | 1 -> e
                                            | s -> e * (pow e (s - 1))
                                          pow a b
                                  | _ -> 000000
                              

  calc expr

