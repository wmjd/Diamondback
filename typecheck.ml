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

let rec tc_e (e : expr) (env : (string * typ) list) (def_env : def_env) : typ =
  match e with
  | ENumber(_) -> TNum
  | EBool(_) -> TBool
  (* TODO *)
  | _ -> failwith "Not yet implemented"
    
let tc_def def_env (DFun(name, args, ret_typ, body)) =
  (* TODO *)
  failwith "Not yet implemented"

let tc_p (defs, main) def_env : typ =
  begin ignore (List.map (tc_def def_env) defs); tc_e main [("input", TNum)] def_env end
