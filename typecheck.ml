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

let type_mismatch str = failwith ("Type mismatch " ^ str)

let tc_e (e : expr) (tenv : (string * typ) list) (def_env : def_env) : typ =
  let rec tc tenv e= 
    let tc_prim2 op = 
      match op with
      | (Plus, e1, e2) | (Minus, e1, e2) | (Times, e1, e2) -> (
        match (tc tenv e1, tc tenv e2) with
        | (TNum, TNum) -> TNum
        | _ -> type_mismatch "prim2 arith")
      | (Less, e1, e2) | (Greater, e1, e2) -> (
        match (tc tenv e1, tc tenv e2) with
        | (TNum, TNum) -> TBool
        | _ -> type_mismatch "prim2 compare")
      | (Equal, e1, e2) -> (
        match (tc tenv e1, tc tenv e2) with
        | (t1, t2) when t1 = t2 -> TBool
        | _ -> type_mismatch "prim2 equal")
    in let tc_prim1 op = 
      match op with 
  	| (Add1, x) | (Sub1, x) -> (
        match (tc tenv x) with
        | TNum -> TNum
        | _ -> type_mismatch "prim1 arith")
     | (IsNum, x) | (IsBool, x) -> (
        match (tc tenv x) with
        | _ -> TBool )
    in let rec tc_body body env =
      let rec aux body =
        match body with
        | [] -> failwith "tc_body Error: empty body (should have been detected in parsing)"
        | [e] -> tc env e
        | e::more -> let _ = tc env e in aux more
      in aux body
    in let rec ext_tenv binding (env : (string * typ) list) = 
      match binding with
      | [] -> env
      | (x, e)::more -> (ext_tenv more ((x, tc env e)::env))
    in match e with
    | EPrim1(prim, arg1) -> tc_prim1 (prim, arg1)
    | EPrim2(prim, arg1, arg2) -> tc_prim2 (prim, arg1, arg2)
    | EBool _ -> TBool
    | ENumber _ -> TNum
    | EIf(pred, then_expr, else_expr) -> (
      match (tc tenv pred, tc tenv then_expr, tc tenv else_expr) with
      | (TBool, t1, t2) when t1 = t2 -> t1
      | _ -> type_mismatch "EIf")
    | EId(x) -> (
      match (find tenv x) with
      | Some(t) -> t
      | _ -> type_mismatch "EId" )
    | ESet(x, e) -> (
      match (tc tenv (EId x), tc tenv e) with
      | (t1, t2) when t1 = t2 -> t1
      | _ -> type_mismatch "ESet")
    | EWhile(pred, body) -> (
      match (tc tenv pred, tc_body body tenv) with
      | TBool, t -> t
      | _ -> type_mismatch "EWhile")
    | ELet(binding, body) ->
      let new_tenv = ext_tenv binding tenv
      in tc_body body new_tenv
    | EApp (f, args) -> (
      let formal, ret = ( match find def_env f with
        | Some(arg_tys, ret_ty) -> arg_tys, ret_ty
        | _ -> type_mismatch "EApp" ) 
      in let actual = List.map (tc tenv) args
      in let rec tc_args = function
        | (a::az, b::bz) when a = b -> tc_args (az, bz)
        | ([], []) -> ret
        | ([], b::bs) -> type_mismatch "EApp"
        | (a::az, []) -> type_mismatch "EApp"
      in tc_args (formal, actual) )
  in tc tenv e
    
let rec tc_body body tenv denv =
    let rec aux body =
      match body with
      | [] -> failwith "tc_body Error: empty body (should have been detected in parsing)"
      | [e] -> tc_e e tenv denv
      | e::more -> let _ = tc_e e tenv denv in aux more
    in aux body

let tc_def def_env (DFun(name, args, ret_typ, body)) =
  match tc_body body (args @ init_tenv) def_env with
  | t when t = ret_typ -> ()
  | _ -> type_mismatch ("definition for " ^ name) 

let tc_p (defs, main) def_env : typ =
  begin ignore (List.map (tc_def def_env) defs); tc_e main init_tenv def_env end
