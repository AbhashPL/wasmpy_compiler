open Lib.Expr
open Lib.Instruction

type FunctionsEnv = string * (type list * type)
type BodyEnv = string * type


let rec tcExpr = 
  ....

let rec tcStmts stmts = 
  ....

let tcProgram program = 
  ....