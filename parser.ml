open Lexer

type basety = S | D | W | L
            | Z                 (* TODO: where should Z go? *)
type extty =                    (* extended types *)
  | B | H

type ty = BaseTy of basety
        | ExtTy of extty
        | AggTy of string       (* aggregated type *)

type instr = Assign of qbe * basety * instr (* dest * type * instr *)
           (* op arg1, arg2, arg3 *)
           | Instr3 of string * qbe * qbe * qbe
           | Instr2 of string * qbe * qbe   (* op arg1, arg2 *)
           | Instr1 of string * qbe         (* op arg1 *)
           | Phi of (qbe * qbe) list  (* phi  *)
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
  | t when t = tok -> ignore (next_token ls)
  | _ -> failwith "expected ..."

let get_op (kw : token) =
  match kw with
  | Keyword(op) -> op
  | _ -> failwith "should have been a keyword"

let get_arg (ident : token) : qbe =
  match ident with
  | Ident(arg) -> Ident(arg)
  | Integer(n) -> Number(n)
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
      ignore (next_token ls);
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
            ignore (next_token ls);
            let arg2 = next_token ls in
            Assign(Ident(dest), get_rettype rettype,
                   Instr2(op, get_arg arg1, get_arg arg2))
          end
        else
          Assign(Ident(dest), get_rettype rettype,
                 Instr1(op, get_arg arg1))
    end
  | Keyword(op) ->
    begin
      ignore (next_token ls);
      let arg1 = get_arg (next_token ls) in
      if (peek_token ls) = Comma
      then begin
        ignore (next_token ls);
        let arg2 = get_arg (next_token ls) in
        if peek_token ls = Comma
        then
          begin
            ignore (next_token ls);
            let arg3 = get_arg (next_token ls) in
            Instr3(op, arg1, arg2, arg3)
          end
        else Instr2(op, arg1, arg2)
      end
      else
        Instr1(op, arg1)
    end
  | _ -> failwith "expected keyword or ident"


let is_phi_instr i =
  match i with
  | Assign(_, _, Phi(_)) -> true
  | _ -> false

(* last instruction of block is always a jump or return(by definiton
   of basic block) *)
let is_last_instr i =
  match i with
  | Instr1("ret", _) | Instr3("jnz", _, _, _) -> true
  | _ -> false
(*
BLOCK :=
    @IDENT    # Block label
    PHI*      # Phi instructions
    INST*     # Regular instructions
    JUMP      # Jump or return
 *)
let parse_block ls =
  let rec parse_phi_instrs acc =
    let next_instr = (parse_instruction ls) in
    if is_phi_instr next_instr
    then parse_phi_instrs (next_instr :: acc)
    else (List.rev acc, next_instr) in
  (* Regular instructions *)
  let rec parse_reg_instrs acc =
    let next_instr = (parse_instruction ls) in
    if is_last_instr next_instr
    then (List.rev acc, next_instr)
    else parse_reg_instrs (next_instr :: acc)
  in
  match peek_token ls with
  | Ident(label) ->
    begin
      ignore (next_token ls);
      (* phi instructions *)
      let (phis, next_instr) = parse_phi_instrs [] in
      let (instrs, last_instr) = parse_reg_instrs [] in
      let instrs = next_instr :: instrs in
      Block(label, phis, instrs, last_instr)
    end
  | _ -> failwith "expected block label"


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
