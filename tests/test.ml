open OUnit2
open Test_lexer
open Test_parser
open Test_cfg


let suite =
"suite">:::
["func_test">:: sum_test;
 "block_test">:: block_test;
 "instr_test">:: instruction_test;
 "typedef_test">:: typedef_test;
 "datadef_test">:: datadef_test;
 "datadef_test2">:: datadef_test2;
 "test_unreachable_code_elim">:: test_unreachable_elim;
 "test_de_ssa">:: test_de_ssa;
 
 "lexer">:: test_lexer;
  ]


let () =
  run_test_tt_main suite
