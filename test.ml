open OUnit2
open Test_lexer
open Test_parser


let suite =
"suite">:::
["func_test">:: sum_test;
 "block_test">:: block_test;
 "instr_test">:: instruction_test;
 "typedef_test">:: typedef_test;
 "datadef_test">:: datadef_test;
 "datadef_test2">:: datadef_test2;
 
 "lexer">:: test_lexer;
  ]


let () =
  run_test_tt_main suite
