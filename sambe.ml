open ExtLib

let compile_to_file ls oc =
  let parsed_func = Qbe_parser.parse_function ls true in
  let dessad = Cfg.de_ssa parsed_func in
  let compiled = X86.asm_of_func dessad in

  Printf.fprintf oc "%s" compiled;
  close_out oc

let () =
  (* TODO: document CLI usage; have flags for specifying params
     instead of relying solely on arg. order *)
  assert ((Array.length Sys.argv - 1) = 2);

  let infile = Sys.argv.(1) in
  let outfile = Sys.argv.(2) in
  let ls = Qbe_lexer.open_stream infile in
  let oc = open_out outfile in
  compile_to_file ls oc
