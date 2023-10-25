(* AST nodes for the WASM opcodes *)

open Printf

type arg = 
  | Const of int

type instruction = 
  | I32Const of int
  | IGlobal of string * int (* Add global variable support in wasmpy *)
  | ILocal
  | ILocalGet of int   (* for getting local vars *)
  | ILocalSet of int
  | IAdd
  | ISub
  | IMul
  
  | IEq
  | INEq
  | IGt
  | ILt
  | IGe
  | ILe
  
  | IAnd
  | IOr
  | IShl
  | IShr
  | IXor


  | ICall of string
  | IBlock of string
  | ILoop of string
  | IEnd
  | IBr of string 
  | IBrIf of string
  | IComment of string


(* let rec arg_to_asm (a : arg) : string = 
  match a with
  | Const(n) -> sprintf "%d" n *)

let i_to_asm (i : instruction) : string =
  match i with
  | I32Const(n) -> 
    sprintf "i32.const %d" n
  | ILocal ->
    sprintf "(local i32)"
  | ILocalGet(n) -> 
    sprintf "local.get %d" n
  | ILocalSet(n) ->
    sprintf "local.set %d" n
  | IAdd ->
    sprintf "i32.add "
  | ISub ->
    sprintf "i32.sub "
  | IMul ->
    sprintf "i32.mul "
  | IEq  -> 
    sprintf "i32.eq " 
  | INEq -> 
    sprintf "i32.ne "
  | IGt  -> 
    sprintf "i32.gt_s "
  | ILt  -> 
    sprintf "i32.lt_s "
  | IGe  -> 
    sprintf "i32.ge_s "
  | ILe  -> 
    sprintf "i32.le_s "
  | IAnd -> 
    sprintf "i32.and "
  | IOr  -> 
    sprintf "i32.or "
  | IShl -> 
    sprintf "i32.shl "
  | IShr -> 
    sprintf "i32.shr "
  | IXor ->
    sprintf "i32.xor "
  | ICall(s) ->
    sprintf "call $%s" s
  | IBlock(s) ->
    sprintf "(block $%s \n" s
  | ILoop(s) ->
    sprintf "(loop $%s \n" s
  | IEnd ->
    sprintf ")\n"
  | IGlobal(name, value) -> 
    sprintf "(global $%s (i32.const %d))" name value
  | IBr(lbl) ->
    sprintf "br $%s" lbl
  | IBrIf(lbl) ->
    sprintf "br_if $%s" lbl
  | IComment(comment) ->
    sprintf "\n;; %s" comment
  
let rec to_wat (is : instruction list) : string =
  match is with
  | [] -> "\n"
  | inst1::rest -> sprintf "%s\n\t%s" (i_to_asm inst1) (to_wat rest)