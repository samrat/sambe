open OUnit2
open Parser

let stream_of_string = Lexer.stream_of_string;;

let instruction_test _ =
  let instr_stream = 
    stream_of_string "%n1 =w phi @start %num, @loop1 %n2" in
  let instr_expected = Assign (FuncIdent "n1", W,
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
  let block_expected = Block ("@loop",
 [Assign (FuncIdent "n1", W,
   Phi
    [(BlockLabel "start", FuncIdent "num");
     (BlockLabel "loop1", FuncIdent "n2")]);
  Assign (FuncIdent "s0", W,
   Phi [(BlockLabel "start", Number 0); (BlockLabel "loop1", FuncIdent "s1")])],
 [Assign (FuncIdent "n2", W, Instr2 ("sub", FuncIdent "n1", Number 1));
  Assign (FuncIdent "c", W, Instr2 ("cslew", FuncIdent "n1", Number 0))],
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
            [Block ("@start", [], [], Instr1 ("jmp", BlockLabel "@loop"));
             Block ("@loop",
                    [Assign (FuncIdent "n1", W,
                             Phi
                               [(BlockLabel "start", FuncIdent "num");
                                (BlockLabel "loop1", FuncIdent "n2")]);
                     Assign (FuncIdent "s0", W,
                             Phi
                               [(BlockLabel "start", Number 0); (BlockLabel "loop1", FuncIdent "s1")])],
                    [Assign (FuncIdent "n2", W, Instr2 ("sub", FuncIdent "n1", Number 1));
                     Assign (FuncIdent "c", W, Instr2 ("cslew", FuncIdent "n1", Number 0))],
                    Instr3 ("jnz", FuncIdent "c", BlockLabel "end", BlockLabel "loop1"));
             Block ("@loop1", [],
                    [Assign (FuncIdent "idx0", L, Instr1 ("extsw", FuncIdent "n2"));
                     Assign (FuncIdent "idx1", L, Instr2 ("mul", Number 4, FuncIdent "idx0"));
                     Assign (FuncIdent "idx2", L,
                             Instr2 ("add", FuncIdent "idx1", FuncIdent "arr"));
                     Assign (FuncIdent "w", W, Instr1 ("loadw", FuncIdent "idx2"));
                     Assign (FuncIdent "s1", W, Instr2 ("add", FuncIdent "w", FuncIdent "s0"))],
                    Instr1 ("jmp", BlockLabel "loop"))]) in
  assert_equal (parse_function sum_stream true) sum_expected


