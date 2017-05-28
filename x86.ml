type reg =
  | RAX
  | RSP
  | RBP

type size =
  | QWORD_PTR
  | DWORD_PTR
  | WORD_PTR
  | BYTE_PTR

type arg =
  | Const of int
  | HexConst of int
  | Reg of reg
  | RegOffset of int * reg
  | Sized of size * arg
  (* pseudo-arg for before reg. allocation *)
  | Var of string

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | ICmp of arg * arg
            
  | IJne of string
  | IJmp of string

  | ILabel of string
