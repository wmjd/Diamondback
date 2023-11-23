open Printf
open Expr
open Asm
open Typecheck

let rec find ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
    if y = x then Some(v) else find rest x

let rec find_def p x =
  match p with
  | [] -> None
  | (DFun(name, args, ret, body) as d)::rest ->
    if name = x then Some(d) else find_def rest x

let stackloc si = RegOffset(-8 * si, RSP)

let true_const  = HexConst(0x0000000000000002L)
let false_const = HexConst(0x0000000000000000L)
let tag_mask =    HexConst(0xFFFFFFFFFFFFFFFEL)

let rec well_formed_e (e : expr) (env : (string * int) list) : string list =
  let dummy_val = 42
  in let rec ext_env b = 
    match b with
    | [] -> []
    | (x, _)::more -> (x, dummy_val)::(ext_env more)  
  in let check_duplicates b =
    let rec dup b x =
      match b with
      | [] -> []
      | (x_prime, v)::more -> if x_prime = x then ["Multiple bindings for variable identifier " ^ x] else dup more x  
    in let rec walk b env = 
      match b with 
      | [] -> []
      | (x, v)::more -> (dup more x) @ (well_formed_e v env) @ (walk more ((x, dummy_val)::env)) 
    in walk b env
  in let well_formed_body body env =
    let rec aux body =
      match body with
      | [] -> failwith "well_formed_body Error: empty body (should have been detected in parsing)"
      | [e] -> well_formed_e e env
      | e::more -> (well_formed_e e env) @ (aux more)
    in aux body
  in match e with
  | ENumber(_)
  | EBool(_) -> []
  | ELet(binding, body) -> (check_duplicates binding) @ (well_formed_body body ((ext_env binding) @ env)) 
  | EId(x) -> (
    match find env x with
    | None -> ["Variable identifier " ^ x ^ " unbound"] 
    | Some(_) -> [] )
  | EIf(predicate, if_branch, else_branch) -> (well_formed_e predicate env) @ (well_formed_e if_branch env) @ (well_formed_e else_branch env)
  | EPrim1(_, arg1) -> (well_formed_e arg1 env)
  | EPrim2(_, arg1, arg2) -> (well_formed_e arg1 env) @ (well_formed_e arg2 env)
  | ESet(x, e) -> (
    match find env x with
    | None -> ["Variable identifier " ^ x ^ " unbound"] 
    | Some(_) -> [] ) @ well_formed_e e env 
  | EWhile(predicate, body) -> (well_formed_e predicate env) @ (well_formed_body body env)
  | EApp (f, args) -> List.flatten (List.map (fun e -> well_formed_e e env) args)

let well_formed_def (DFun(name, args, ret, body)) =
  (* TODO *)
  failwith "Not yet implemented: well_formed_def"

let well_formed_prog (defs, main) =
  (* TODO *)
  (List.concat (List.map well_formed_def defs)) @ (well_formed_e main [("input", 1)])

let check p : string list =
  match well_formed_prog p with
  | [] -> []
  | errs -> failwith (String.concat "\n" errs)

let rec compile_expr (e : expr) (si : int) (env : (string * int) list) def_env
  : instruction list =
  (* TODO *)
  failwith "Not yet implemented: compile_expr"

and compile_prim1 op e si env def_env =
  (* TODO *)
  failwith "Not yet implemented: compile_prim1"

and compile_prim2 op e1 e2 si env def_env =
  (* TODO *)
  failwith "Not yet implemented: compile_prim2"

and compile_def (DFun(name, args, ret, body)) def_env =
  (* TODO *)
  failwith "Not yet implemented: compile_def"

let compile_to_string ((defs, main) as prog : Expr.prog) =
  let _ = check prog in
  let def_env = build_def_env defs in
  let _ = tc_p prog def_env in
  let compiled_defs = List.concat (List.map (fun d -> compile_def d defs) defs) in
  let compiled_main = compile_expr main 2 [("input", 1)] defs in
  let prelude = "  section .text\n" ^
                "  extern error\n" ^
                "  extern print\n" ^
                "  global our_code_starts_here\n" in
  let kickoff = "our_code_starts_here:\n" ^
                "push rbx\n" ^
                "  mov [rsp - 8], rdi\n" ^ 
                to_asm compiled_main ^
                "\n  pop rbx\nret\n" in
  let postlude = []
    (* TODO *) in
  let as_assembly_string = (to_asm (compiled_defs @ postlude)) in
  sprintf "%s%s\n%s\n" prelude as_assembly_string kickoff
