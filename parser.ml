open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr

let boa_max = int_of_float(2.**62.) - 1;;
let boa_min = -int_of_float(2.**62.);;
let valid_id_regex = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let number_regex = Str.regexp "^[+-]?[0-9]+"
let reserved_words = ["let"; "add1"; "sub1"; "isNum"; "isBool"; "if"; "set"; "while"; "def"; "print"]
let reserved_constants = ["true"; "false"; ]
let int_of_string_opt s =
  try Some(int_of_string s) with
  | _ -> None

let rec parse (sexp : Sexp.t) =
  (* TODO *) failwith "Not yet implemented: parse"

and parse_binding binding =
  (* TODO *) failwith "Not yet implemented: parse_binding"

let parse_def sexp =
  (* TODO *) failwith "Not yet implemented: parse_def"

let rec parse_program sexps =
  match sexps with
  | [] -> failwith "Invalid: Empty program"
  | [e] -> ([], parse e)
  | e::es ->
     let parse_e = (parse_def e) in
     let defs, main = parse_program es in
     parse_e::defs, main
