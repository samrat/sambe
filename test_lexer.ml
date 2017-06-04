open OUnit2
open Qbe_lexer

let test_lexer _ =
  let str = "function data %n1 =w phi @start %num, @loop1 %n2" in
  let stream = stream_of_string str in
  assert_equal (next_token stream) (Some(Keyword("function")));
  assert_equal (next_token stream) (Some(Keyword("data")));
  assert_equal (next_token stream) (Some(Ident("%n1")));
  assert_equal (next_token stream) (Some(Equals));
  assert_equal (next_token stream) (Some(Ident("w")));
  assert_equal (next_token stream) (Some(Keyword("phi")));
  assert_equal (next_token stream) (Some(Ident("@start")));
  assert_equal (next_token stream) (Some(Ident("%num")));
  assert_equal (next_token stream) (Some(Comma));
  assert_equal (next_token stream) (Some(Ident("@loop1")));
  assert_equal (next_token stream) (Some(Ident("%n2")));
