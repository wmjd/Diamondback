open Runner
open Expr
open Printf
open OUnit2

(* Fill in `myTestList` with your own tests. There are two ways to add a test:
 *
 * (1) By adding the code of the program as a string. In this case, add an entry
 *     of this form in `myTestList`:
 *
 *     t <test_name> <program_code> <result>
 *
 * (2) By adding a test inside the 'input/' folder and adding an entry for it
 *     in `myTestList`. The entry in this case should be:
 *
 *     t_file <test_name> <file_name> <result>
 *     
 *     Where name is the name of the file inside 'input/' with the extension
 *     ".ana". For example:
 *
 *     t_file "myTest" "mytest.ana" "6";
 *)

let t_i name program expected args = name>::test_run program name expected args
let t name program expected = name>::test_run program name expected []
let terr_i name program expected args = name>::test_err program name expected args
let t_err name program expected = name>::test_err program name expected []
let t_parse name program expected =
  name>::(fun _ -> assert_equal expected (Runner.parse_string program));;

let f_to_s fname = Runner.string_of_file ("input/" ^ fname)
let t_file name program expected = (t name (f_to_s program) expected);;
let t_err_file name program expected = (t_err name (f_to_s program) expected);;
let t_i_file name program expected args = (t_i name (f_to_s program) expected args)

let myTestList =
  [ (* Fill in your tests here: *)
	t_file "42" "42.boa" "42";
	t_file "43" "43.boa" "43";
	t_file "99" "99.boa" "99";
	t_file "999" "999.boa" "999";
	t_file "body" "body.boa" "3";
	t_file "43" "43.boa" "43";	
	t_file "errerr" "errerr.boa" "true";
	t_file "errerr2" "errerr2.boa" "true";
	t_err_file "failty" "failty.boa" "Type mismatch prim1 arith";
	t_file "if" "if.boa" "1";
	t_err_file "if1" "if1.boa" "Type mismatch EIf";
	t_file "ifTrue" "ifTrue.boa" "1";
	t_err_file "ifTyErr" "ifTyErr.boa" "Type mismatch EIf";
	t_file "loop" "loop.boa" "false";
	t_err_file "mismatch" "mismatch.boa" "Type mismatch prim2 arith";
	t_err_file "of" "of.boa" "Non-representable number";
	t_err_file "reserved_word" "reserved_word.boa" "Syntax error: let is a reserved word";
	t_file "set" "set.boa" "2";
	t_file "shadow" "shadow.boa" "1";
	t_err_file "well_form" "well_form.boa" (
		"Multiple bindings for variable identifier x\n" ^
		"Multiple bindings for variable identifier y\n" ^
		"Variable identifier foo unbound");
	t_err_file "well_form2" "well_form2.boa" "Multiple bindings for variable identifier y";
	t_err_file "well_form_err" "well_form_err.boa" "Variable identifier x unbound";
	t_err_file "wfbe" "wfbe.boa" "Variable identifier z unbound";
	t_file "notgt" "notgt.boa" "false";
	t_file "gt" "gt.boa" "true";
	t_file "lt" "lt.boa" "true";
	t_file "loopx" "loopx.boa" "0";
	t_file "loop42" "loop42.boa" "42";
	t_file "loop0" "loop0.boa" "0";
	t_i_file "fibonacci" "fibonacci.boa" "5" ["4"]
  ]
;;
