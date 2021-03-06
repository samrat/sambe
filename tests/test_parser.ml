open OUnit2
open Qbe_parser

let stream_of_string = Qbe_lexer.stream_of_string;;

let typedef_test _ =
  let stream =
    stream_of_string "type :cryptovector = align 16 { w 4 }" in
  let expected =
    TypeDef (AggType "cryptovector", [(BaseTy W, 4)], Some 16) in
  assert_equal (parse_typedef stream) expected

let datadef_test _ =
  let stream =
    stream_of_string "data $a = { w 1 2 3, b 0 }" in
  let expected =
    DataDef (false, GlobalIdent "a",
             [(BaseTy W, [Integer 1; Integer 2; Integer 3]); (ExtTy B, [Integer 0])]) in
  assert_equal (parse_datadef stream false) expected

let datadef_test2 _ =
  let stream =
    stream_of_string "data $b = { z 1000 }" in
  let expected =
    DataDef (true, GlobalIdent "b", [(BaseTy Z, [Integer 1000])]) in
  assert_equal (parse_datadef stream true) expected

let instruction_test _ =
  let instr_stream = 
    stream_of_string "%n1 =w phi @start %num, @loop1 %n2" in
  let instr_expected = Assign (FuncIdent "n1", BaseTy W,
                               Phi
                                 [(BlockLabel "start", FuncIdent "num");
                                  (BlockLabel "loop1", FuncIdent "n2")]) in
  assert_equal (parse_instruction instr_stream) instr_expected

let block_test _ =
  let block_stream = stream_of_string "@loop
	%n1 =w phi @start %num, @loop1 %n2
	%s0 =w phi @start 0, @loop1 %s1
	%n2 =w sub %n1, 1
	%c =w cslew %n1, 0
jnz %c, @end, @loop1" in
  let block_expected = Block (BlockLabel("loop"),
 [Assign (FuncIdent "n1", BaseTy W,
   Phi
    [(BlockLabel "start", FuncIdent "num");
     (BlockLabel "loop1", FuncIdent "n2")]);
  Assign (FuncIdent "s0", BaseTy W,
   Phi [(BlockLabel "start", Integer 0); (BlockLabel "loop1", FuncIdent "s1")])],
 [Assign (FuncIdent "n2", BaseTy W, Instr2 ("sub", FuncIdent "n1", Integer 1));
  Assign (FuncIdent "c", BaseTy W, Instr2 ("cslew", FuncIdent "n1", Integer 0))],
                              Instr3 ("jnz", FuncIdent "c", BlockLabel "end", BlockLabel "loop1"))
  in
  assert_equal (parse_block block_stream) block_expected


let sum_test _ =
  let sum_stream = stream_of_string "function w $sum(l %arr, w %num) {
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
}" in
  let sum_expected = 
    FunDef (true, BaseTy W, GlobalIdent "sum",
            [(BaseTy L, FuncIdent "arr"); (BaseTy W, FuncIdent "num")],
            [Block (BlockLabel "start", [], [], Instr1 ("jmp", BlockLabel "loop"));
             Block (BlockLabel "loop",
                    [Assign (FuncIdent "n1", BaseTy W,
                             Phi
                               [(BlockLabel "start", FuncIdent "num");
                                (BlockLabel "loop1", FuncIdent "n2")]);
                     Assign (FuncIdent "s0", BaseTy W,
                             Phi
                               [(BlockLabel "start", Integer 0); (BlockLabel "loop1", FuncIdent "s1")])],
                    [Assign (FuncIdent "n2", BaseTy W, Instr2 ("sub", FuncIdent "n1", Integer 1));
                     Assign (FuncIdent "c", BaseTy W, Instr2 ("cslew", FuncIdent "n1", Integer 0))],
                    Instr3 ("jnz", FuncIdent "c", BlockLabel "end", BlockLabel "loop1"));
             Block (BlockLabel "loop1", [],
                    [Assign (FuncIdent "idx0", BaseTy L, Instr1 ("extsw", FuncIdent "n2"));
                     Assign (FuncIdent "idx1", BaseTy L, Instr2 ("mul", Integer 4, FuncIdent "idx0"));
                     Assign (FuncIdent "idx2", BaseTy L,
                             Instr2 ("add", FuncIdent "idx1", FuncIdent "arr"));
                     Assign (FuncIdent "w", BaseTy W, Instr1 ("loadw", FuncIdent "idx2"));
                     Assign (FuncIdent "s1", BaseTy W, Instr2 ("add", FuncIdent "w", FuncIdent "s0"))],
                    Instr1 ("jmp", BlockLabel "loop"));
            Block (BlockLabel "end", [], [], Instr1 ("ret", FuncIdent "s0"))]) in
  assert_equal (parse_function sum_stream true) sum_expected


