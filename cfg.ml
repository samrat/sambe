open Parser

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
