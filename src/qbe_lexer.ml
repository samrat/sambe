open Util
open Instr

type token = Ident of string
           | Integer of Int64.t
           | Float of float     (* TODO: f32 and f64 *)
           | Double of float
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

let read_char ls =
  let c = Stream.next ls.stm in
  let _ = ls.column <- ls.column + 1 in
  if c = '\n'
  then begin
    (ls.line_num <- ls.line_num + 1);
    (ls.column <- 1);
    c
  end
  else c
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
      | "type" | "function" | "data" | "align" -> Keyword(id)
      | _ when is_instruction_op id -> Keyword(id)
      | f -> (match explode f with
          | 's'::'_'::flt -> Float(float_of_string (implode flt))
          | 'd'::'_'::flt -> Double(float_of_string (implode flt))
          | _ -> Ident(id))
    end
  | _ -> failwith "expected ident"

let read_int ls neg =
  let rec read_int' (acc : Int64.t) : Int64.t =
    match peek_char ls with
    | Some c ->
      if is_digit c
      then
        let _ = read_char ls in
        read_int' Int64.(add (mul acc (of_int 10)) (of_int ((Char.code c) - (Char.code '0'))))
      else
        acc
    | None -> acc
  in
  if neg
  then begin
    ignore (read_char ls);
    Integer(Int64.(mul (of_int (-1)) (read_int' zero)))
  end
  else Integer(read_int' (Int64.zero))

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
    let _ = ls.next_token <- (next_token ls) in
    ls.next_token
  | Some(tok) -> Some(tok)
and next_token ls =
  match ls.next_token with
  | Some(tok) ->
    let _ = ls.next_token <- None
    in Some(tok)
  | None ->
    begin
      skip_whitespace ls;
      match peek_char ls with
      | Some(c) when is_ident_start c -> Some(read_ident ls)
      | Some(c) when is_alpha c -> Some(read_keyword ls)
      | Some(c) when is_digit c -> Some(read_int ls false)
      | Some('-') -> Some(read_int ls true)
      | Some('"') -> Some(read_string ls)
      | Some('#') ->
        let _ = skip_comment ls in
        next_token ls
      | Some(c) -> Some(read_symbol ls c)
      | None -> None
    end

(*

[Keyword("data"); Ident("$str"); Equals; LBrace; Ident("b"); String("hello world"); Comma;  Ident("b"); Integer(0); RBrace]

*)
