open Parser
open Util
open ExtLib

module Qbe_set = Set.Make (struct
    type t = qbe
    let compare = compare
  end);;

let dedup xs =
  Qbe_set.elements (Qbe_set.of_list xs)

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


let find_reachable_nodes (cfg : (qbe, qbe list) Hashtbl.t) (start : qbe) =
  let rec traverse reachable current =
    if (List.mem current reachable)
    then reachable
    else match Hashtbl.find_option cfg current with
        | Some(succs) -> List.flatten (List.map (traverse (current::reachable))
                                         succs)
        | None -> (current::reachable)
  in
  dedup (traverse [] start)

let eliminate_unreachable_blocks fn =
  let (export, retty, name, params, blocks) = match fn with
    | FunDef(export, retty, name, params, blocks) -> 
      (export, retty, name, params, blocks)
    | _ -> failwith "expected function definition" in
  let (entry, all_nodes, g) = build_cfg blocks in
  let reachable_nodes = find_reachable_nodes g entry in
  let new_blocks = List.filter (fun block ->
      match block with
      | Block(label, _, _, _) when List.mem label reachable_nodes ->
        true
      | _ -> false) blocks in
  FunDef(export, retty, name, params, new_blocks)

(* Hashtbl.find_option g (BlockLabel "loop1");; *)

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
  let new_blocks = List.fold_left (fun new_blocks block ->
      match block with
      | Block(label, phis, instrs, jmp) ->
        let block_movs = Hashtbl.find_default block_movs label [] in
        let new_instrs = List.map triple_to_assign block_movs in
        let new_block = Block(label, [], instrs @ new_instrs, jmp) in
        new_block :: new_blocks
      | _ -> failwith "expected block"
    )
      []
      blocks
                   |> List.rev
  in FunDef(export, retty, name, params, new_blocks)


(*
let parsed_func = parse_function func true;;
let blocks = match parsed_func with
  | FunDef(_, _, _, _, blocks) -> blocks
  | _ -> failwith "expected function definition";;
let (entry, all_nodes, g) = build_cfg blocks;;
let pg = pred_graph g entry;;
let doms = dom_solver pg all_nodes (BlockLabel("start"))
*)
