open Qbe_parser
open ExtLib
open Printf

module Ty_set = Set.Make (struct
    type t = ty
    let compare = compare
  end);;

(* Types accepted by each instruction *)
let ops =
  [
    (* Arithmetic and logic *)
    ("add", [W;L;S;D], [W;L;S;D]);
    ("sub", [W;L;S;D], [W;L;S;D]);
    ("mul", [W;L;S;D], [W;L;S;D]);
    ("div", [W;L;S;D], [W;L;S;D]);
    ("rem", [W;L], [W;L]);
    ("udiv", [W;L], [W;L]);
    ("urem", [W;L], [W;L]);
    ("and", [W;L], [W;L]);
    ("xor", [W;L], [W;L]);
    ("sar", [W;L], [W]);
    ("shr", [W;L], [W]);
    ("shl", [W;L], [W]);

    (* Comparisons *)
    ("ceqw", [W], [W]);
    ("cnew", [W], [W]);
    ("csgew", [W], [W]);
    ("csgtw", [W], [W]);
    ("cslew", [W], [W]);
    ("csltw", [W], [W]);
    ("cugew", [W], [W]);
    ("cugtw", [W], [W]);
    ("culew", [W], [W]);
    ("cultw", [W], [W]);

    ("ceql", [L], [L]);
    ("cnel", [L], [L]);
    ("csgel", [L], [L]);
    ("csgtl", [L], [L]);
    ("cslel", [L], [L]);
    ("csltl", [L], [L]);
    ("cugel", [L], [L]);
    ("cugtl", [L], [L]);
    ("culel", [L], [L]);
    ("cultl", [L], [L]);

    ("ceqs", [S], [S]);
    ("cnes", [S], [S]);
    ("cges", [S], [S]);
    ("cgts", [S], [S]);
    ("cles", [S], [S]);
    ("clts", [S], [S]);
    ("cos", [S], [S]);
    ("cuos", [S], [S]);

    ("ceqd", [D], [D]);
    ("cned", [D], [D]);
    ("cged", [D], [D]);
    ("cgtd", [D], [D]);
    ("cled", [D], [D]);
    ("cltd", [D], [D]);
    ("cod", [D], [D]);
    ("cuod", [D], [D]);

    (* TODO: add remaining instructions *)
  ]

let optab = Hashtbl.create 32
let () = List.iter (fun (op, arg1_types, arg2_types) ->
    Hashtbl.add optab op
      (Ty_set.of_list (List.map (fun t -> (BaseTy(t))) arg1_types),
       Ty_set.of_list (List.map (fun t -> (BaseTy(t))) arg2_types)))
    ops

(* If this function returns, it means `func` has typechecked. The
   first type error that it encounters will cause a failure(via
   `failwith`) *)
let typecheck_func func =
  let env : (string, ty) Hashtbl.t = Hashtbl.create 32 in
  let get_arg_type arg =
    match arg with
    | FuncIdent(id) -> (match Hashtbl.find_option env id with
        | Some(found) -> found
        | None -> failwith (Printf.sprintf "%s not defined" id))
    (* TODO: distinguish between 32- and 64-bit values *)
    | Integer(i) -> BaseTy(L)
    | Float(f) -> BaseTy(S)
    | Double(d) -> BaseTy(D)
    | _ -> failwith (Printf.sprintf "NYI: %s" (ExtLib.dump arg))
  in
  let check_type (typ : ty) (exp : Ty_set.t) =
    Ty_set.mem typ exp
  in
  let rec typecheck_instr instr =
    match instr with
    | Assign(FuncIdent(dest), typ, src_instr) ->
      (Hashtbl.add env dest typ);
      typecheck_instr src_instr
    | Instr0(op) -> ()
    | Instr1(op, FuncIdent(arg)) -> (match (Hashtbl.find_option env arg) with
        | Some(arg_typ) -> ()
        | None -> failwith (sprintf "invalid argument for %s: %s" op (ExtLib.dump arg)))
    | Instr1(op, _) -> ()     (* TODO: ? *)
    | Instr2(op, arg1, arg2) ->
      let (arg1_type, arg2_type) = (get_arg_type arg1, get_arg_type arg2) in
      let (exp1, exp2) = Hashtbl.find optab op in
      (match (check_type arg1_type exp1, check_type arg2_type exp2) with
       | (true, true) -> ()
       | (true, false) -> failwith (sprintf "invalid second arg in %s" op)
       | (false, true) -> failwith (sprintf "invalid first arg in %s" op)
       | (false, false) -> failwith (sprintf "invalid first and second args in %s" op)
      )
    | Instr3(op, arg1, arg2, arg3) -> ()
    | Phi(pargs) ->
      ignore (List.fold_left (fun seen_blocks (label, value)  ->
          let label = (match label with
            | BlockLabel(l) -> l
            | _ -> failwith "expected BlockLabel") in
          if (List.mem label seen_blocks)
          then failwith (Printf.sprintf "multiple entries for @%s in phi" label)
          else (label::seen_blocks))
        []
        pargs);
      ()
    | Call(func_name, args) -> () (* TODO *)
    | _ -> failwith (Printf.sprintf "NYI: %s" (ExtLib.dump instr))
  in
  let typecheck_block block =
  match block with
    | Block(label, phis, reg_instrs, last_instr) ->
      let t_phis = (List.map typecheck_instr phis) in
      let t_regs = (List.map typecheck_instr reg_instrs) in
      t_phis @ t_regs
  | _ -> failwith "expected Block"
  in
  match func with
  | FunDef(_, _, name, args, blocks) ->
    ignore (List.map (fun (typ, param) ->
        (match param with
         | FuncIdent(param) -> Hashtbl.add env param typ
         | _ -> failwith "expected FuncIdent")) args);
    List.map typecheck_block blocks
    |> List.flatten
  | _ -> failwith "expected FunDef"
