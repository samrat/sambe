open ExtLib

let () =
  (* TODO: document CLI usage; have flags for specifying params
     instead of relying solely on arg. order *)
  assert ((Array.length Sys.argv - 1) = 2);

  let infile = Sys.argv.(1) in
  let outfile = Sys.argv.(2) in
  let ls = Qbe_lexer.open_stream infile in
  let oc = open_out outfile in
  X86.compile_to_file ls oc
