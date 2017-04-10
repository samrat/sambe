open OUnit2
open Lexer

let test_lexer _ =
  let str = "function data %n1 =w phi @start %num, @loop1 %n2" in
  let stream = stream_of_string str in
  assert_equal (next_token stream) (Keyword("function"));
  assert_equal (next_token stream) (Keyword("data"));
  assert_equal (next_token stream) (Ident("%n1"));
  assert_equal (next_token stream) Equals;
  assert_equal (next_token stream) (Ident("w"));
  assert_equal (next_token stream) (Keyword("phi"));
  assert_equal (next_token stream) (Ident("@start"));
  assert_equal (next_token stream) (Ident("%num"));
  assert_equal (next_token stream) Comma;
  assert_equal (next_token stream) (Ident("@loop1"));
  assert_equal (next_token stream) (Ident("%n2"))
