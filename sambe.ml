open ExtLib

let compile_to_file ls oc =
  let parsed_func = Qbe_parser.parse_function ls true in
  let dessad = Cfg.de_ssa parsed_func in
  let compiled = X86.asm_of_func dessad in

  Printf.fprintf oc "%s" compiled;
  close_out oc

(* TODO: make an actual CLI *)
let () =
  let ls = Qbe_lexer.open_stream "test.ssa" in
  let oc = open_out "test.s" in
  compile_to_file ls oc
