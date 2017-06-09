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

val peek_token : lexer_state -> token option
val next_token : lexer_state -> token option
val open_stream : string -> lexer_state
val stream_of_string: string -> lexer_state
