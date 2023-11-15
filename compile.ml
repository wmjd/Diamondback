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

let rec well_formed_e (e : expr) (env : (string * int) list) : string list =
  match e with
  | ENumber(_)
  | EBool(_) -> []
  (* TODO *)
  | _ -> failwith "Not yet implemented"

let well_formed_def (DFun(name, args, ret, body)) =
  (* TODO *)
  failwith "Not yet implemented"

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
  failwith "Not yet implemented"

and compile_prim1 op e si env def_env =
  (* TODO *)
  failwith "Not yet implemented"

and compile_prim2 op e1 e2 si env def_env =
  (* TODO *)
  failwith "Not yet implemented"

and compile_def (DFun(name, args, ret, body)) def_env =
  (* TODO *)
  failwith "Not yet implemented"

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
