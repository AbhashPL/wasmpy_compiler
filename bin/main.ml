open Compiler
open Printf

let () = 
  let wat = (compile_to_string test5) in
  printf "%s\n" wat