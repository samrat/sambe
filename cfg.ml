open Qbe_parser
open Util
open ExtLib

open X86

module Qbe_set = Set.Make (struct
    type t = qbe
    let compare = compare
  end);;

let build_cfg blocks =
  let graph : (qbe, qbe list) Hashtbl.t = Hashtbl.create 12 in
  let all_block_labels = List.map (fun block ->
      match block with
      | Block(label, _, _, _) -> label
      | _ -> failwith "function body has zero blocks")
      blocks
  in
  let entry = List.hd all_block_labels in
  let rec build_cfg' blocks =
    match blocks with
    | Block(label, _ , _, Instr1("jmp", dest)) :: rest ->
      (Hashtbl.add graph label [dest]);
      build_cfg' rest
    | Block(label, _ , _, Instr3("jnz", _, dest1, dest2)) :: rest ->
      (Hashtbl.add graph label [dest1; dest2]);
      build_cfg' rest
    | Block(label, _, _, Instr1("ret", _)) :: rest ->
      build_cfg' rest
    | [] -> graph
    | _ -> failwith "NYI"
  in
  (entry, all_block_labels, build_cfg' blocks)

(* Reverse-postorder traversal of blocks *)
let order_blocks cfg start =
  let order = ref [] in
  let rec traverse reachable current =
    if (List.mem current reachable)
    then reachable
    else
      let ret = match Hashtbl.find_option cfg current with
      | Some(succs) -> 
        List.flatten (List.map (traverse (current::reachable))
                                  succs)
      | None -> (current::reachable)
      in
      order := (current :: !order);
      ret
  in
  ignore (traverse [] start);
  !order

let eliminate_unreachable_blocks fn =
  let (export, retty, name, params, blocks) = match fn with
    | FunDef(export, retty, name, params, blocks) -> 
      (export, retty, name, params, blocks)
    | _ -> failwith "expected function definition" in
  let (entry, all_nodes, g) = build_cfg blocks in
  let reachable_nodes = order_blocks g entry in
  let new_blocks = List.filter (fun block ->
      match block with
      | Block(label, _, _, _) when List.mem label reachable_nodes ->
        true
      | _ -> false) blocks in
  FunDef(export, retty, name, params, new_blocks)

let pred_graph cfg start =
  let graph = Hashtbl.create 12 in
  let visited = Hashtbl.create 12 in

  let rec build_graph node =
    match Hashtbl.find_option visited node with
    | None ->
      Hashtbl.add visited node true;
      (match Hashtbl.find_option cfg node with
       | Some(succs) ->
         ignore (List.map (fun s -> 
             let old = Hashtbl.find_option graph s in
             match old with
             | Some(old_preds) ->
               Hashtbl.replace graph s (node::old_preds)
             | None ->
               Hashtbl.add graph s [node])
             succs);
         ignore (List.map build_graph succs);
         ()
       | None -> ()
      )
    | Some(_) -> ()
  in
  build_graph start;
  graph


(* Simple fixed-point iterative solver for dominators *)
let dom_solver preds (all_nodes : qbe list) start =
  let doms : (qbe, qbe list) Hashtbl.t = Hashtbl.create 16 in
  let rec step () =
    let changed = (List.fold_left (fun changed i ->
        let pred_doms = (List.map (fun x ->
            Hashtbl.find doms x)
            (Hashtbl.find_default preds i []))
        in
        let pred_dom_intersection =
          List.fold_left (fun acc ds ->
              Qbe_set.inter acc (Qbe_set.of_list ds))
            (Qbe_set.of_list (if pred_doms = []
                              then []
                              else all_nodes))
            pred_doms
        in
        let temp' = (Qbe_set.add i pred_dom_intersection) in
        let temp = (Qbe_set.elements temp') in
        if temp = Hashtbl.find doms i
        then changed
        else begin
          Hashtbl.replace doms i temp;
          true
        end)
        false
        all_nodes) in
    if changed
    then step ()
    else ()
  in 
  ignore (List.map (fun n -> Hashtbl.add doms n all_nodes) all_nodes);
  Hashtbl.replace doms start [start];
  step ();
  doms

(* NOTE: this is the naive SSA back translation. *)
(* TODO: Use Brigg's algorithm instead, in order to avoid "lost copy"
   problem *)
(* Remove phi instructions from all blocks. It does so by placing an
   assign in the block from where the variable would be inherited.
*)
let de_ssa fn =
  let (export, retty, name, params, blocks) = match fn with
    | FunDef(export, retty, name, params, blocks) -> 
      (export, retty, name, params, blocks)
    | _ -> failwith "expected function definition"
  in
  let all_phis = List.fold_left (fun all_phis block ->
      match block with
      | Block(_, phis, reg_instrs, jmp) -> phis @ all_phis
      | _ -> failwith "expected block")
    []
    blocks
  in
  let movs_to_add_in_block = (List.fold_left (fun acc instr ->
      match instr with
      | Assign(dest, ty, Phi(vars)) ->
        (dest, ty, vars) :: acc
      | _ -> failwith "expected phi instrs")
      []
      all_phis)
  in
  let block_to_movs = Hashtbl.create 32 in
  let block_movs = List.fold_left (fun acc (dest, ty, vs) ->
      ignore (List.map (fun (block_label, vars) -> 
          let block_movs = Hashtbl.find_default acc block_label [] in
          Hashtbl.replace acc block_label ((dest, ty, vars) :: block_movs))
      vs);
      acc)
      block_to_movs
      movs_to_add_in_block
  in
  let triple_to_assign (dest, ty, var) =
    Assign(dest, ty, Instr1("copy", var)) in
  let new_blocks =
    List.fold_left (fun new_blocks block ->
        match block with
        | Block(label, phis, instrs, jmp) ->
          let block_movs = Hashtbl.find_default block_movs label [] in
          let new_instrs = List.map triple_to_assign block_movs in
          let new_block = Block(label, [], instrs @ new_instrs, jmp) in
          new_block :: new_blocks
        | _ -> failwith "expected block" )
      []
      blocks
    |> List.rev
  in FunDef(export, retty, name, params, new_blocks)


let get_arg_val = function
  | FuncIdent(id) -> Var(id)
  | Integer(i) -> Const(i)
  | _ -> failwith "NYI"

let get_label = function
  | BlockLabel(label) -> label
  | _ -> failwith "expected BlockLabel"

let rec instr_to_x86 instr =
  match instr with
  | Assign(dest, ty, src_instr) ->
    instr_to_x86 src_instr @
    [ IMov(get_arg_val dest, Reg(RAX))]
  | Instr1(op, arg) ->
    begin
      match op with
      | "copy" -> 
        [ IMov(Reg(RAX), get_arg_val arg) ]
      | "jmp" ->
        [ IJmp(get_label arg) ]
      | _ -> failwith "NYI"
    end
  | Instr2(op, arg1, arg2) ->
    begin
      match op with
      | "add" -> 
        [ IMov(Reg(RAX), get_arg_val arg1);
          IAdd(Reg(RAX), get_arg_val arg2); ]
      | _ -> failwith "NYI"
    end
  | Instr3(op, arg1, arg2, arg3) ->
    begin
      match op with
      | "jnz" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          ICmp(Const(0), Reg(RAX));
          IJne(get_label arg3);
          (* TODO: Can we avoid this jump by generating code for the
             `arg2` block here? *)
          IJmp(get_label arg2);
        ]
      | _ -> failwith "NYI"
    end
    
  | _ -> failwith "NYI"

let block_to_x86 block =
  match block with
  | Block(label, [], reg_instrs, last_instr) ->
    [ ILabel(get_label label) ] @
    List.flatten (List.map instr_to_x86 reg_instrs) @
    (instr_to_x86 last_instr)
  | Block(_, phis, _, _) -> failwith "expected empty phi instr list"
  | _ -> failwith "NYI"


(* TODO: Currently, this puts everything in memory. Implement the
   register allocation pass to fix that. *)
let assign_homes (func: instruction list) =
  let mappings : (string, arg) Hashtbl.t = Hashtbl.create 16 in
  let counter = ref 0 in
  let find_loc v =
    match Hashtbl.find_option mappings v with
    | Some(loc) -> loc
    | None ->
      let new_loc = RegOffset(-8 * !counter, RBP) in
      Hashtbl.add mappings v new_loc;
      counter := !counter + 1;
      new_loc
  in
  let replace_instr2_args_with_locs = function
  | Var(dest), Var(src) ->
    let dest_loc = find_loc dest in
    let src_loc = find_loc src in
    (dest_loc, src_loc)
  | Var(dest), src ->
    let dest_loc = find_loc dest in
    (dest_loc, src)
  | dest, Var(src) ->
    let src_loc = find_loc src in
    (dest, src_loc)
  | dest, src ->
    (dest, src)
  in
  let fixup_instr = function
    | IMov(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      IMov(new_dest, new_src)
    | IAdd(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      IAdd(new_dest, new_src)
    | ICmp(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      ICmp(new_dest, new_src)
    | instr -> instr
  in
  List.map fixup_instr func

(*
let parsed_func = parse_function func true;;
let blocks = match parsed_func with
  | FunDef(_, _, _, _, blocks) -> blocks
  | _ -> failwith "expected function definition";;
let (entry, all_nodes, g) = build_cfg blocks;;
let pg = pred_graph g entry;;
let doms = dom_solver pg all_nodes (BlockLabel("start"))
*)
