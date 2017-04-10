open OUnit2
open Test_lexer
open Test_parser


let suite =
"suite">:::
["func_test1">:: sum_test;
 "block_test1">:: block_test;
 "instr_test1">:: instruction_test;

 "lexer">:: test_lexer;
  ]


let () =
  run_test_tt_main suite
