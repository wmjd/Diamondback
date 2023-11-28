open Expr
open Printf

let rec find ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
    if y = x then Some(v) else find rest x

type def_env = (string * (typ list * typ)) list

let build_def_env (defs : def list) : def_env =
  let get_typ (DFun(name, args, ret_typ, body)) = (name, ((List.map snd args), ret_typ)) in
  List.map get_typ defs

let init_tenv = [("input", TNum)]
let ext_tenv (args : (string * typ) list) (tenv : (string * typ) list) = args @ tenv 

let type_mismatch str = failwith ("Type mismatch " ^ str)

let rec tc_e (e : expr) (tenv : (string * typ) list) (def_env : def_env) : typ =
  match e with
  | ENumber(_) -> TNum
  | EBool(_) -> TBool
  (* TODO *)
  | _ -> failwith "Not yet implemented: tc_e"
    
let rec tc_body body tenv denv =
    let rec aux body =
      match body with
      | [] -> failwith "tc_body Error: empty body (should have been detected in parsing)"
      | [e] -> tc_e e tenv denv
      | e::more -> let _ = tc_e e tenv denv in aux more
    in aux body

let tc_def def_env (DFun(name, args, ret_typ, body)) =
  match tc_body body (ext_tenv args init_tenv) def_env with
  | t when t = ret_typ -> ()
  | _ -> type_mismatch ("definition for " ^ name) 

let tc_p (defs, main) def_env : typ =
  begin ignore (List.map (tc_def def_env) defs); tc_e main init_tenv def_env end
