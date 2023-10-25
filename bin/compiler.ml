open Printf
open Lib.Expr
open Lib.Instruction


(* TODO: If..else wasm, write wat and see how if..else works*)

(* TODO: Add functions to wasmpy *)

(* TODO: Read about how function types are expressed in ocaml and just read about types in ocaml
  ; stuff about it being a typed stack machine
   *)

(* Some global refs *)
let count = ref 0
let num_locals = ref 0


let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count


(* We want to be using an imported JS function to deal with error output here. *)
let throw_err code = 
  [
    (* add instr to Push error code the stack *)
    ICall("error");
  ]

(* Labels for functions that will handle runtime errors *)
let error_overflow = "error_overflow"
let error_non_int  = "error_non_int"
let error_non_bool = "error_non_bool"
let error_arity_mismatch = "error_arity_mismatch"



let rec lookup_id key env = 
  match env with
  | [] -> failwith "identifier accessed without defining it"
  | (str, idx)::rest -> 
    if (str = key) then idx else (lookup_id key rest)

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | []    -> false
    | x::xs -> (elt = x) || (find_one xs elt)
    

(* goal of compiling exprs is to put the result at the top of stack *)
let rec compile_expr (e : expr) (env : (string * int) list) : instruction list =
  match e with
  | ENumber(n) -> [ I32Const(n) ]
  
  | ETrue -> [ I32Const(1) ]

  | EFalse -> [ I32Const(0) ]
  
  | EId(name) -> 
    let idx = (lookup_id name env) in
    [ ILocalGet(idx) ]

  | EBuiltin1(bi1, e) -> 
    let e' = compile_expr e env in
    (match bi1 with
    | Print -> e' @ [ICall("logNum"); I32Const(0)])
  
  | EOperator(op, e1, e2) ->
    let e1' = compile_expr e1 env in
    let e2' = compile_expr e2 env in
    let op' = 
      (match op with
      | Add -> [ IAdd ]
      | Sub -> [ ISub ]
      | Mul -> [ IMul ]
      | Equal    -> [ IEq ]
      | NotEqual -> [ INEq ]
      | Greater -> [ IGt ]
      | Less -> [ ILt ]
      | Is -> failwith " not implemented python 'is' operator "
      | GreaterOrEqual -> [ IGe ] 
      | LessOrEqual -> [ ILe ]
      )
    in
    e1' @ e2' @ op'
  | ECall(name, args) -> 
    failwith "complete function call"
  

let rec compile_stmts stmts local_idx env =
  match stmts with
  | [] -> []
  | s::rest -> 
    begin
      match s with
      | StAssign(name,exp) -> 
        
        let var_names = List.map fst env in
        (if ((find_one var_names name) = false) then num_locals := !num_locals + 1);
        let new_env = if ((find_one var_names name) = false) then ((name,local_idx)::env) else env in
        let new_local_idx = if ((find_one var_names name) = false) then (local_idx + 1) else local_idx in
        let our_locals_idx = lookup_id name new_env in
        let exp' = (compile_expr exp env) in
        let exp'' = exp' @ [ (ILocalSet(our_locals_idx)) ] in
        exp'' @ (compile_stmts rest new_local_idx new_env)

      | StExpr(exp) -> (compile_expr exp env) @ (compile_stmts rest local_idx env)
      
      | StReturn(exp) -> 
        failwith "implement this in tandom with functions"

      | StIf(cond, thn_block, els_block) -> 
        failwith "implement this after while "

      | StWhile(loopcond, body_block) -> 
        let loopcond' = (compile_expr loopcond env) in
        let loopbody = (compile_stmts body_block local_idx env) in
        let block_lbl = (gen_temp "block1") in
        let loop_lbl = (gen_temp "loop1") in
        [IBlock(block_lbl); ILoop(loop_lbl)]
        @
        [IComment("Condition check")]
        @ 
        loopcond'
        @
        [I32Const(1); IXor]
        @
        [IBrIf(block_lbl)]
        @
        [IComment(" Loop Body")]
        @
        loopbody
        @
        [IBr(loop_lbl); IEnd; IEnd]
        @
        (compile_stmts rest local_idx env)
        
    end

let rec declare_local_vars nlocals = 
  if(nlocals <> 0) then
      [ILocal] @ (declare_local_vars (nlocals - 1))
    else 
      []

let compile_def def = 
  match def with
  | DFun(name, args, body_stmts) ->
    failwith "lots left to do here"
    (compile_stmts body_stmts 0 [])


let rec compile_funcdefs defs = 
  match defs with
  | [] -> []
  | def::rest -> 
      (compile_def def) @ (compile_funcdefs rest)

let compile_program program = 
  match program with
  | Program(funcdefs, main_stmts) ->
    (compile_funcdefs funcdefs) @ (compile_stmts main_stmts 0 [])


let compile_to_string program =
  let ilist = compile_program program in
  let wat_string = to_wat ilist in
  let nlocals = !num_locals in
  let locals =  to_wat @@ (declare_local_vars nlocals) in
  sprintf
  "(module
    (import \"console\" \"logNum\" (func $logNum (param i32)))

    (func (export \"_start\") (result i32)
    %s
    %s
    )
  )" locals wat_string




(* ==================================================  Test Cases =================================================== *)


let test1 = StExpr(EOperator(Add, ENumber(2), EOperator(Sub, ENumber(5), ENumber(4))))

let test2 = StExpr(EBuiltin1(Print, 
  EOperator(Add, ENumber(2), 
    EOperator(Sub, ENumber(5), ENumber(4)))))

let test3 = Program([] , [StAssign("a", EOperator(Add, ENumber(2), EOperator(Sub, ENumber(5), ENumber(4)))); 
                          StExpr(EBuiltin1(Print, EId("a")))])

let test4 = Program([], [ StAssign("sum", ENumber(0))
                        ; StAssign("sum", EOperator(Add, EId("sum"), ENumber(2)))
                        ; StExpr(EBuiltin1(Print, EId("sum")))
                        ])

let test5 = Program([], [StAssign("sum", ENumber(0))
                        ; StAssign("i", ENumber(10))
                        ; StWhile(EOperator(Greater, EId("i"), ENumber(0)), 
                          [ StAssign("sum", EOperator(Add, EId("sum"), ENumber(1))); StAssign("i", EOperator(Sub, EId("i"), ENumber(1))); ])
                        ; StExpr(EBuiltin1(Print, EId("sum")))
                        ])