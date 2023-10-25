(* AST nodes for the parsed python src *)

(* type type =
  | Int
  | Bool
  | None *)

type op =
  | Add
  | Sub
  | Mul
  | Equal (* Implement this as "is" carefully*)
  | NotEqual
  | Greater
  | Less
  | Is
  | GreaterOrEqual
  | LessOrEqual


type built_in1 = 
  | Print

type expr = 
  | ENumber of int
  | ETrue
  | EFalse
  | EId of string
  | EBuiltin1 of built_in1 * expr
  | EOperator of op * expr * expr
  | ECall of string * expr list

type stmt = 
  | StAssign of string * expr
  | StExpr of expr
  | StReturn of expr
  | StIf of expr * stmt list * stmt list
  | StWhile of expr * stmt list

type funcdef = 
  | DFun of string * string list * stmt list


type program =
  | Program of funcdef list * stmt list (* list of defs + main_body *)