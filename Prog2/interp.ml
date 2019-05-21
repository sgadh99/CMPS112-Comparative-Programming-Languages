(* $Id: interp.ml,v 1.7 2019-01-29 17:26:15-08 - - $ *)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> (match memref with 
                    | Arrayref (ident, index) -> Array.get(Hashtbl.find Tables.array_table ident) (int_of_float(eval_expr index))
                    | Variable ident -> Hashtbl.find Tables.variable_table ident)
    | Unary (oper, expr) -> let x = (Hashtbl.find Tables.unary_fn_table oper)
                                  in x (eval_expr (expr))
    | Binary (oper, expr1, expr2) -> let y = (Hashtbl.find Tables.binary_fn_table oper)
    in y (eval_expr (expr1)) (eval_expr (expr2))

(* let rec eval_binary oper expr1 expr2 =
     let y = (Hashtbl.find binary_fn_table oper)
     in y eval_expr (expr1) eval_expr (expr2) *)

(* let interp_if (label : Absyn.label) (expr : Absyn.expr) (expr : Abysn.expr) ()
     let value = eval_expr(expr)
          (* apply binary operator 
          then go to label (using label table) *)
           *)


let interp_goto (label: Absyn.label) : Absyn.program option =      
      Some (Hashtbl.find Tables.label_table label)   

(* let interp_goto (label: Absyn.label) = match label with
     | String label -> Hashtbl.find Tables.label_table label *)
     (* | None -> None  *)

let interp_dim (ident : Absyn.ident) (expr: Absyn.expr) =

     (* let(size, arr) = (eval_expr expr, Array.make_float 0.0);; *)
     (* let size = eval_expr expr *)
     (* let arr = Array.make_float size 0.0;; *)
     (*let arr = (Array.make_float (size 0.0))
          in arr (eval_expr (expr))*)

     Hashtbl.add Tables.array_table ident (Array.make (int_of_float(eval_expr expr)) 0.)
     (* in arr (eval_expr (expr));; *)
     (* Hashtbl.add Tables.array_table ident *)
     (* let value = (Hashtbl.add Tables.array_table ident) in value (eval_expr (ident)) (eval_expr (value)) *)
     
let interp_let (memref: Absyn.memref) (expr: Absyn.expr) =
     let value = eval_expr expr 
     in match memref with
     | Arrayref (ident, index) -> 
          let tharray = Hashtbl.find Tables.array_table ident in
               Array.set tharray ((int_of_float (eval_expr index))) value
     | Variable (ident) ->
          Hashtbl.add Tables.variable_table ident value

let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())

let interp_input (memref_list : Absyn.memref list) =
    let input_number memref =
        try  let number = Etc.read_number ()
             in (print_float number; print_newline ())
        with End_of_file -> 
             (print_string "End_of_file"; print_newline ())
    in List.iter input_number memref_list

 let interp_stmt (stmt : Absyn.stmt) = match stmt with 
          | Dim (ident, expr) -> interp_dim ident expr; None
          | Let (memref, expr) -> interp_let memref expr; None
          | Goto label -> interp_goto label
          | If (expr, label) -> unimpl "If (expr, label)"
          | Print print_list -> interp_print print_list; None
          | Input memref_list -> interp_input memref_list; None
     
    (* | Dim (ident, expr) -> Hashtbl.add Tables.array_table ident (Array.make 0 (eval_expr expr))  *)
    
    

let interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> let next_line = interp_stmt stmt in match next_line with 
          | None -> interpret otherlines
          | Some line -> interpret line
      (* | _, _, Some stmt -> let ret = interp_stmt stmt *)
          (* in match ret with
               | Some line -> interpret line
               | None -> interpret otherLine   *)
      
let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

