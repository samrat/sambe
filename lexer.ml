type token = Ident of string
           | Integer of int
           | Float of float     (* TODO: f32 and f64 *)
           | Keyword of string
           | String of string
           | LParen | RParen
           | LBrace | RBrace
           | Comma
           | Pound
           | Equals

type lexer_state = { mutable line_num: int; mutable column: int;
                     stm: char Stream.t; mutable next_token: token option }

let open_stream file = { line_num = 1; column = 1;
                         stm = Stream.of_channel (open_in file);
                         next_token = None
                       }

let stream_of_string s = { line_num = 1; column = 1;
                           stm = Stream.of_string s;
                           next_token = None
                         }
(* TODO: Update line_num and column *)
let read_char ls =
  Stream.next ls.stm
let peek_char ls = Stream.peek ls.stm

let is_digit c = let code = Char.code c in
  code >= Char.code('0') && code <= Char.code('9')
let is_alpha c = let code = Char.code c in
  code >= Char.code('A') && code <= Char.code('Z') ||
  code >= Char.code('a') && code <= Char.code('z')

let is_ident_start c =
  match c with
  | '$' | ':' | '@' | '%' -> true
  | _ -> false

let is_valid_ident_char =
  function
  | '_' | '.' | '-' -> true
  | c when is_digit c -> true
  | _ -> false

let read_ident ls =
  let buf = Buffer.create 32 in
  let rec read_ident' () =
    match peek_char ls with
    | Some c ->
      if is_alpha c || is_ident_start c || is_valid_ident_char c
      then
        let _ = read_char ls in
        let _ = (Buffer.add_char buf c) in
        read_ident' ()
      else Ident(Buffer.to_bytes buf)
    | None -> Ident(Buffer.to_bytes buf)
  in read_ident' ()

let read_keyword ls =
  match read_ident ls with
  | Ident(id) -> begin
      match id with
      (* TODO: add all other instructions *)
      | "type" | "function" | "data"
      | "phi"
      | "call" | "ret" | "sub" | "storel" -> Keyword(id)
      | _ -> Ident(id)
    end
  | _ -> failwith "expected ident"

let read_int ls =
  let rec read_int' acc =
    match peek_char ls with
    | Some c ->
      if is_digit c
      then
        let _ = read_char ls in
        read_int' (acc*10 + (Char.code c - Char.code '0'))
      else
        Integer(acc)
    | None -> Integer(acc)
  in
  read_int' 0

let rec skip_whitespace ls =
  match peek_char ls with
  | Some(' ') | Some('\n') | Some('\t') ->
    let _ = read_char ls in
    skip_whitespace ls
  | Some(c) -> ()
  | None -> ()

let read_symbol ls c =
  let sym = match c with
    | '(' -> LParen
    | ')' -> RParen
    | '{' -> LBrace
    | '}' -> RBrace
    | '=' -> Equals
    | ',' -> Comma
    | _ -> failwith "invalid symbol"
  in
  let _ = read_char ls in
  sym

let read_string ls =
  let buf = Buffer.create 128 in
  let _ = read_char ls in       (* consume starting quote *)
  let rec read_string' () =
    match peek_char ls with
    | Some c ->
      if c = '"'
      then
        let _ = read_char ls in (* consume ending quote *)
        String(Buffer.to_bytes buf)
      else
        let _ = read_char ls in
        let _ = (Buffer.add_char buf c) in
        read_string' ()
    | None -> String(Buffer.to_bytes buf)
  in read_string' ()

let rec skip_comment ls =
  match peek_char ls with
  | Some('\n') ->
    let _ = read_char ls in
    ()
  | Some(c) ->
    let _ = read_char ls in
    skip_comment ls
  | None -> ()


let rec peek_token ls =
  match ls.next_token with
  | None ->
    let _ = ls.next_token <- Some(next_token ls) in
    begin
    match ls.next_token with
    | Some(tok) -> tok
    | None -> failwith "no more tokens"
    end
  | Some(tok) -> tok
and next_token ls =
  match ls.next_token with
  | Some(tok) ->
    let _ = ls.next_token <- None
    in tok
  | None ->
    begin
      skip_whitespace ls;
      match peek_char ls with
      | Some(c) when is_ident_start c -> read_ident ls
      | Some(c) when is_alpha c -> read_keyword ls
      | Some(c) when is_digit c -> read_int ls
      | Some('"') -> read_string ls
      | Some('#') ->
        let _ = skip_comment ls in
        next_token ls
      | Some(c) -> read_symbol ls c
      | None -> failwith "No more tokens"
    end
(*

[Keyword("data"); Ident("$str"); Equals; LBrace; Ident("b"); String("hello world"); Comma;  Ident("b"); Integer(0); RBrace]

*)
