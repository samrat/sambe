open OUnit2
open Cfg
open Qbe_parser

let stream_of_string = Qbe_lexer.stream_of_string

let test_unreachable_elim _ =
  let func = stream_of_string "function w $sum(l %arr, w %num) {
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
@unreachable
        %idx0 =l extsw %n2
        %idx1 =l mul 4, %idx0
        %idx2 =l add %idx1, %arr
        %w =w loadw %idx2
        %s1 =w add %w, %s0
        jmp @loop
@end
        ret %s0
}" in
  let parsed_func = parse_function func true in
  (* block "unreachable" should have been removed *)
  let expected =
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
   [Assign (FuncIdent "n2", BaseTy W,
     Instr2 ("sub", FuncIdent "n1", Integer 1));
    Assign (FuncIdent "c", BaseTy W,
     Instr2 ("cslew", FuncIdent "n1", Integer 0))],
   Instr3 ("jnz", FuncIdent "c", BlockLabel "end", BlockLabel "loop1"));
  Block (BlockLabel "loop1", [],
   [Assign (FuncIdent "idx0", BaseTy L, Instr1 ("extsw", FuncIdent "n2"));
    Assign (FuncIdent "idx1", BaseTy L,
     Instr2 ("mul", Integer 4, FuncIdent "idx0"));
    Assign (FuncIdent "idx2", BaseTy L,
     Instr2 ("add", FuncIdent "idx1", FuncIdent "arr"));
    Assign (FuncIdent "w", BaseTy W, Instr1 ("loadw", FuncIdent "idx2"));
    Assign (FuncIdent "s1", BaseTy W,
     Instr2 ("add", FuncIdent "w", FuncIdent "s0"))],
   Instr1 ("jmp", BlockLabel "loop"));
  Block (BlockLabel "end", [], [], Instr1 ("ret", FuncIdent "s0"))])
  in
  assert_equal (eliminate_unreachable_blocks parsed_func)
    expected


let test_de_ssa _ =
  let func = stream_of_string "function w $foo() {
@ifstmt
        %x =l copy 0
        jnz %x, @ift, @iff
@ift
        jmp @retstmt
@iff
        jmp @retstmt
@retstmt
        %y =w phi @ift 1, @iff 2
        ret %y
}" in
  let parsed_func = parse_function func true in
  let dessad = de_ssa parsed_func in
  let expected = FunDef(true, BaseTy W, GlobalIdent "foo", [],
   [Block (BlockLabel "ifstmt", [],
     [Assign (FuncIdent "x", BaseTy L, Instr1 ("copy", Integer 0))],
     Instr3 ("jnz", FuncIdent "x", BlockLabel "ift", BlockLabel "iff"));
    Block (BlockLabel "ift", [],
     [Assign (FuncIdent "y", BaseTy W, Instr1 ("copy", Integer 1))],
     Instr1 ("jmp", BlockLabel "retstmt"));
    Block (BlockLabel "iff", [],
     [Assign (FuncIdent "y", BaseTy W, Instr1 ("copy", Integer 2))],
     Instr1 ("jmp", BlockLabel "retstmt"));
    Block (BlockLabel "retstmt", [], [], Instr1 ("ret", FuncIdent "y"))])
  in
  assert_equal expected dessad
