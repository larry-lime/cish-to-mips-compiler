(* Compile Cish AST to MIPS AST *)
open Mips
open Ast

exception IMPLEMENT_ME

type result = { code : Mips.inst list; data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0

let new_int () =
  label_counter := !label_counter + 1;
  !label_counter

let new_label () = "L" ^ string_of_int (new_int ())

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make (struct
  type t = Ast.var

  let compare = String.compare
end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref VarSet.empty

(* generate a fresh temporary variable and store it in the variables set. *)
(*NOTE: Likely used in collect vars*)
let rec new_temp () =
  let t = "T" ^ string_of_int (new_int ()) in
  (* make sure we don't already have a variable with the same name! *)
  if VarSet.mem t !variables then new_temp ()
  else (
    variables := VarSet.add t !variables;
    t)

(* reset internal state *)
let reset () =
  label_counter := 0;
  variables := VarSet.empty

(* find all of the variables in a program and add them to
 * the set variables *)
(* NOTE: This return unit (which is like None) *)
let rec collect_vars_exp ((e, _) : exp) =
  match e with
  | Int _ -> ()
  | Var x -> variables := VarSet.add x !variables
  | Binop (e1, _, e2) ->
      collect_vars_exp e1;
      collect_vars_exp e2
  | Not e -> collect_vars_exp e
  | And (e1, e2) | Or (e1, e2) ->
      collect_vars_exp e1;
      collect_vars_exp e2
  | Assign (x, e) ->
      variables := VarSet.add x !variables;
      collect_vars_exp e
  | _ -> raise IMPLEMENT_ME

let rec collect_vars_stmt ((s, _) : stmt) =
  match s with
  | Exp e -> collect_vars_exp e
  | Seq (s1, s2) ->
      collect_vars_stmt s1;
      collect_vars_stmt s2
  | If (e, s1, s2) ->
      collect_vars_exp e;
      collect_vars_stmt s1;
      collect_vars_stmt s2
  | While (e, s) ->
      collect_vars_exp e;
      collect_vars_stmt s
  | For (e1, e2, e3, s) ->
      collect_vars_exp e1;
      collect_vars_exp e2;
      collect_vars_exp e3;
      collect_vars_stmt s
  | Return e -> collect_vars_exp e
  | Let (v, e, s) -> raise IMPLEMENT_ME

(* FIXME: This expression has type program = func list but an expression was expected of type stmt = rstmt * pos*)
let rec collect_vars (p : Ast.program) : unit =
  (*************************************************************)
  collect_vars_stmt p

let rec exp2mips ((e, p) : Ast.exp) : inst list =
  match e with
  | Call (f, args) ->
      (* Example implementation to handle function calls:
         1. Evaluate arguments and push them onto the stack.
         2. Call the function.
         3. Adjust the stack pointer if necessary.
         4. Move the function's return value to the appropriate register (if not already).
      *)
      raise IMPLEMENT_ME
  | Int j -> [ Li (R2, Word32.fromInt j) ]
  | Var x -> [ La (R2, x); Lw (R2, R2, Word32.fromInt 0) ]
  | Binop (e1, b, e2) -> (
      binop_helper e1 e2
      @
      match b with
      | Plus -> [ Add (R2, R2, Reg R3) ]
      | Minus -> [ Sub (R2, R3, R2) ]
      | Times -> [ Mul (R2, R2, R3) ]
      | Div -> [ Div (R2, R3, R2) ]
      | Eq -> [ Seq (R2, R3, R2) ]
      | Neq -> [ Sne (R2, R3, R2) ]
      | Lt -> [ Slt (R2, R3, Reg R2) ]
      | Lte -> [ Sle (R2, R3, R2) ]
      | Gt -> [ Sgt (R2, R3, R2) ]
      | Gte -> [ Sge (R2, R3, R2) ])
  | Assign (x, e) -> exp2mips e @ [ La (R3, x); Sw (R2, R3, Word32.fromInt 0) ]
  | Not e -> exp2mips e @ [ Sgt (R2, R2, R0); Seq (R2, R2, R0) ]
  | Or (e1, e2) ->
      binop_helper e1 e2
      @ [ Sgt (R2, R2, R0); Sgt (R3, R3, R0); Or (R2, R2, Reg R3) ]
  | And (e1, e2) ->
      binop_helper e1 e2
      @ [ Sgt (R2, R2, R0); Sgt (R3, R3, R0); And (R2, R2, Reg R3) ]

and binop_helper e1 e2 =
  let t = new_temp () in
  exp2mips e1
  @ [ La (R3, t); Sw (R2, R3, Word32.fromInt 0) ]
  @ exp2mips e2
  @ [ La (R3, t); Lw (R3, R3, Word32.fromInt 0) ]

let rec compile_stmt ((s, p) : Ast.stmt) : inst list =
  (*************************************************************)
  match s with
  | Exp e -> exp2mips e
  | Seq (s1, s2) -> compile_stmt s1 @ compile_stmt s2
  | If (e, s1, s2) ->
      let else_l = new_label () in
      let end_l = new_label () in
      exp2mips e
      @ [ Beq (R2, R0, else_l) ]
      @ compile_stmt s1 @ [ J end_l; Label else_l ] @ compile_stmt s2
      @ [ Label end_l ]
  | While (e, s) ->
      let test_l = new_label () in
      let top_l = new_label () in
      [ J test_l; Label top_l ] @ compile_stmt s @ [ Label test_l ] @ exp2mips e
      @ [ Bne (R2, R0, top_l) ]
  | For (e1, e2, e3, s) ->
      compile_stmt
        (Seq ((Exp e1, 1), (While (e2, (Seq (s, (Exp e3, 1)), 1)), 1)), 1)
  | Return e -> exp2mips e @ [ Jr R31 ]
  | Let (v, e, s) -> raise IMPLEMENT_ME

(*TODO: Implement compile function*)
let rec compile (p : Ast.program) : result = raise IMPLEMENT_ME

let result2string (res : result) : string =
  let code = res.code in
  let data = res.data in
  let strs = List.map (fun x -> Mips.inst2string x ^ "\n") code in
  let vaR8decl x = x ^ ":\t.word 0\n" in
  let readfile f =
    let stream = open_in f in
    let size = in_channel_length stream in
    let text = Bytes.create size in
    let _ = really_input stream text 0 size in
    let _ = close_in stream in
    text
  in
  let debugcode = readfile "print.asm" in
  "\t.text\n" ^ "\t.align\t2\n" ^ "\t.globl main\n" ^ String.concat "" strs
  ^ "\n\n" ^ "\t.data\n" ^ "\t.align 0\n"
  ^ String.concat "" (List.map vaR8decl data)
  ^ "\n" ^ Bytes.to_string debugcode
