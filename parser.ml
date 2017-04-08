open Lexer

type basety = S | D | W | L
            | Z                 (* TODO: where should Z go? *)
type extty =                    (* extended types *)
  | B | H

type ty = BaseTy of basety
        | ExtTy of extty
        | AggTy of string       (* aggregated type *)

type instr = Assign of qbe * basety * instr (* dest * type * instr *)
           | Instr2 of string * qbe * qbe   (* op arg1, arg2 *)
           | Instr1 of string * qbe         (* op arg1 *)
           | Phi of (string * string) list  (* phi  *)
and qbe =
  (* const *)
  | Number of int
  | Float of float
  | Ident of string

  (* for DataDef *)
  | IdentOffset of string * int

  (* typedef *)
  | TypeDef of
      string *                    (* name *)
      (ty * int) list *           (* fields *)
      int option                  (* alignment *)

  (* datadef *)
  | DataDef of
      bool *                    (* export? *)
      string *                  (* name *)
      (ty * (qbe list)) list    (* dataitems *)

  | FunDef of
      bool *                    (* export? *)
      ty *                      (* return type *)
      string *                  (* name *)
      (ty * string) list *      (* params *)
      qbe list                  (* block list *)

  | Block of
      string *                  (* block name *)
      instr list *              (* phi instrs *)
      instr list *              (* regular instrs *)
      instr                     (* jump/return *)

let expect ls tok =
  match peek_token ls with
  | tok -> begin next_token ls; () end
  | _ -> failwith "expected ..."

let get_op (kw : token) =
  match kw with
  | Keyword(op) -> op
  | _ -> failwith "should have been a keyword"

let get_arg (ident : token) =
  match ident with
  | Ident(arg) -> arg
  | _ -> failwith "should have been an ident"

let get_rettype (tystr : token) =
  match tystr with
  | Ident(ty) ->
    begin
      match ty with
      | "s" -> S
      | "d" -> D
      | "w" -> W
      | "l" -> L
      | _ -> failwith "NYI"
    end
  | _ -> failwith "expected string"


let parse_instruction ls =
  match peek_token ls with
  | Ident(dest) ->
    begin
      next_token ls;
      expect ls Equals;
      let rettype = next_token ls in
      let op = get_op (next_token ls) in
      if op = "phi" then
        let src1 = get_arg (next_token ls) in
        let var1 = get_arg (next_token ls) in
        let _ = expect ls Comma in
        let src2 = get_arg (next_token ls) in
        let var2 = get_arg (next_token ls) in
        Assign(Ident(dest), get_rettype rettype,
               Phi([(src1, var1); (src2, var2)]))
      else
        let arg1 = next_token ls in
        if peek_token ls = Comma
        then
          begin
            next_token ls;
            let arg2 = next_token ls in
            Assign(Ident(dest), get_rettype rettype,
                   Instr2(op, Ident(get_arg arg1), Ident(get_arg arg2)))
          end
        else
          Assign(Ident(dest), get_rettype rettype,
                 Instr1(op, Ident(get_arg arg1)))
    end
  | Keyword(op) ->
    begin
      next_token ls;
      let arg1 = get_arg (next_token ls) in
      if (peek_token ls) = Comma
      then begin
        next_token ls;
        let arg2 = get_arg (next_token ls) in
        Instr2(op, Ident(arg1), Ident(arg2))
      end
      else
        Instr1(op, Ident(arg1))
    end
  | _ -> failwith "NYI"




(*

type :fourfloats = { s, s, d, d }
=> TypeDef("fourfloats", [(BaseTy(S), 1); (BaseTy(S), 1);
                          (S, 1); (D, 1)], None)

type :abyteandmanywords = { b, w 100 }
=> TypeDef("abyteandmanywords", [(B, 1); (W, 100)], None)

type :cryptovector = align 16 { w 4 }
=> TypeDef("cryptovector", [(W, 4)], Some 16)

type :opaque = align 16 { 32 }
=> TypeDef("opaque", [(B, 32)], Some 16)



data $a = { w 1 2 3, b 0 }
=> DataDef(false, "a", [(W, [Number(1); Number(2); Number(3)]);
                        (B, [Number(0)])])

data $b = { z 1000 }
=> DataDef(false, "b", [(Z, [1000])])

data $c = { l -1, l $c }
=>


FunDef(false, BaseTy(W), "foobar", [], [Block("foo", [Phi], [], Ret)]);;

*)
