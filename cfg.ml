open Parser
open ExtLib

module Qbe_set = Set.Make (struct
    type t = qbe
    let compare = compare
  end);;


let func = Lexer.stream_of_string "function w $sum(l %arr, w %num) {
@start
@loop
	%n1 =w phi @start %num, @loop1 %n2
	%s0 =w phi @start 0, @loop1 %s1
	%n2 =w sub %n1, 1
	%c =w cslew %n1, 0
	jnz %c, @end, @loop1
@loop1
	%idx0 =l extsw %n2
	%idx1 =l mul 4, %idx0
	%idx2 =l add %idx1, %arr
	%w =w loadw %idx2
	%s1 =w add %w, %s0
	jmp @loop
@end
	ret %s0
}
";;
(* parse_function func true;; *)

let build_cfg blocks : (qbe, qbe list) Hashtbl.t =
  let graph : (qbe, qbe list) Hashtbl.t = Hashtbl.create 12 in
  let rec build_cfg' blocks =
    match blocks with
    | Block(label, _ , _, Instr1("jmp", dest)) :: rest ->
      (Hashtbl.add graph label [dest]);
      build_cfg' rest
    | Block(label, _ , _, Instr3("jnz", _, dest1, dest2)) :: rest ->
      (Hashtbl.add graph label [dest1; dest2]);
      build_cfg' rest
    | [] -> graph
    | _ -> failwith "NYI"
  in
  build_cfg' blocks

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

let parsed_func = parse_function func true;;
let blocks = match parsed_func with
  | FunDef(_, _, _, _, blocks) -> blocks
  | _ -> failwith "expected function definition";;
let g = build_cfg blocks;;
let pg = pred_graph g (BlockLabel "start");;
let all_nodes = [BlockLabel("start"); BlockLabel("loop"); BlockLabel("loop1"); BlockLabel("end")];;
let doms = dom_solver pg all_nodes (BlockLabel("start"))
