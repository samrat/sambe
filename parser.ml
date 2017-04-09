open Lexer
open ExtLib

(* TODO:
- Handle Blocks ending without a jump/return
   Insert a jump to the next block

- Instead of all identifiers being Ident, there should be different
  constructors for all the different sigils
*)

type basety = S | D | W | L
            | Z                 (* TODO: where should Z go? *)
type extty =                    (* extended types *)
  | B | H

type ty = BaseTy of basety
        | ExtTy of extty
        | AggTy of string       (* aggregated type *)

type ident_ty = BlockIdent | AggType | FuncIdent | GlobalIdent


type instr = Assign of qbe * basety * instr (* dest * type * instr *)
           (* op arg1, arg2, arg3 *)
           | Instr3 of string * qbe * qbe * qbe
           | Instr2 of string * qbe * qbe   (* op arg1, arg2 *)
           | Instr1 of string * qbe         (* op arg1 *)
           | Instr0 of string
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
      qbe *                     (* name *)
      (ty * qbe) list *         (* params *)
      qbe list                  (* block list *)

  | Block of
      string *                  (* block name *)
      instr list *              (* phi instrs *)
      instr list *              (* regular instrs *)
      instr                     (* jump/return *)

let expect ls tok =
  match peek_token ls with
  | t when t = tok -> ignore (next_token ls)
  | t -> failwith (Printf.sprintf "expected %s but got %s" (dump tok) (dump t))

let get_op (kw : token) =
  match kw with
  | Keyword(op) -> op
  | _ -> failwith (Printf.sprintf "expected a keyword: %s" (dump kw))

let get_arg (ident : token) : qbe =
  match ident with
  | Ident(arg) -> Ident(arg)
  | Integer(n) -> Number(n)
  | _ -> failwith "should have been an ident"

let get_type (tystr : token) =
  match tystr with
  | Ident(ty) ->
    begin
      match ty with
      | "s" -> S
      | "d" -> D
      | "w" -> W
      | "l" -> L
      | _ -> failwith (Printf.sprintf "not a type: %s" (dump ty))
    end
  | _ -> failwith "expected string"

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let ident_type (id : token) =
  match id with
  | Ident(id) ->
    begin
    match explode id with
    | '@'::_ -> BlockIdent
    | ':'::_ -> AggType
    | '$'::_ -> GlobalIdent
    | '%'::_ -> FuncIdent
    | _ -> failwith (Printf.sprintf "not a valid identifier sigil: %s" (dump id))
    end
  | _ -> failwith (Printf.sprintf "expected ident but got %s" (dump id))


let parse_instruction ls =
  match peek_token ls with
  | Ident(dest) ->
    begin
      ignore (next_token ls);
      let _ = print_string dest in
      let _ = print_string "      \n" in
      expect ls Equals;
      let rettype = next_token ls in
      let op = get_op (next_token ls) in
      if op = "phi" then
        let src1 = get_arg (next_token ls) in
        let var1 = get_arg (next_token ls) in
        let _ = expect ls Comma in
        let src2 = get_arg (next_token ls) in
        let var2 = get_arg (next_token ls) in
        Assign(Ident(dest), get_type rettype,
               Phi([(src1, var1); (src2, var2)]))
      else
        let arg1 = next_token ls in
        if peek_token ls = Comma
        then
          begin
            ignore (next_token ls);
            let arg2 = next_token ls in
            Assign(Ident(dest), get_type rettype,
                   Instr2(op, get_arg arg1, get_arg arg2))
          end
        else
          Assign(Ident(dest), get_type rettype,
                 Instr1(op, get_arg arg1))
    end
  | Keyword(op) ->
    begin
      ignore (next_token ls);
      if op = "ret" &&
         ident_type (peek_token ls) = BlockIdent
      then Instr0("ret")
      else
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
  | Instr1("jmp", _) | Instr0("ret")
  | Instr1("ret", _) | Instr3("jnz", _, _, _) -> true
  | _ -> false


(* Figure out if block is about to end(next instr is jump/return. Or
   has already ended(next token is new block label or is RBrace) *)
let remaining_block_size ls =
  match peek_token ls with
    | Ident(id) -> if ident_type (Ident(id)) = BlockIdent then Some 0 else None
    | Keyword(kw) -> (match kw with
        | "jmp" | "jnz" | "ret" -> Some 1
        | _ -> None)
    | RBrace -> Some 0
    | _ -> None

(*
BLOCK :=
    @IDENT    # Block label
    PHI*      # Phi instructions
    INST*     # Regular instructions
    JUMP      # Jump or return
 *)
let parse_block ls =
  let rec parse_phi_instrs acc =
    match remaining_block_size ls with
    | None ->
      let next_instr = (parse_instruction ls) in
      if is_phi_instr next_instr
      then parse_phi_instrs (next_instr :: acc)
      else (List.rev acc, Some(next_instr))
    | _ -> (List.rev acc, None)
  in
  (* Regular instructions *)
  let rec parse_reg_instrs acc =
    match remaining_block_size ls with
    | None ->
      let next_instr = (parse_instruction ls) in
      parse_reg_instrs (next_instr :: acc)
    | Some(_) -> List.rev acc
  in
  match peek_token ls with
  | Ident(label) ->
    begin
      ignore (next_token ls);
      (* phi instructions *)
      let (phis, next_instr) = parse_phi_instrs [] in
      let next_instr' = (match next_instr with
          | None -> []
          | Some(i) -> [i]) in
      let instrs = parse_reg_instrs [] in
      let instrs = next_instr' @ instrs in
      let last_instr = parse_instruction ls in
      Block(label, phis, instrs, last_instr)
    end
  | _ -> failwith "expected block label"

let get_params ls =
  let rec get_params' acc =
  match peek_token ls with
  | Ident(t) ->
    begin
      ignore (next_token ls);
      let ty = get_type (Ident(t)) in
      let param = get_arg (next_token ls) in
      let new_params = ((BaseTy(ty), param) :: acc) in
      if peek_token ls = Comma
      then begin
        ignore (next_token ls);
        get_params' new_params
      end
      else begin
        expect ls RParen;
        List.rev new_params
      end
    end
  | tok -> failwith (Printf.sprintf "syntax error in function params: %s"
                     (dump tok))
  in
  ignore (expect ls LParen);
  get_params' []

(*
FUNCDEF :=
    ['export'] 'function' [BASETY | :IDENT] $IDENT PARAMS
    '{'
       BLOCK+
    '}'
*)
let parse_function ls export =
  let rec parse_blocks acc =
    if peek_token ls = RBrace
    then
      begin
        ignore (next_token ls);
        List.rev acc
      end
    else parse_blocks ((parse_block ls) :: acc)
  in

  match peek_token ls with
  | Keyword("function") ->
    begin
      ignore (next_token ls);
      (* TODO: can be either ident or keyword *)
      let rettype = get_type (next_token ls) in
      let name = get_arg (next_token ls) in
      let params = get_params ls in
      let _ = expect ls LBrace in
      let blocks = parse_blocks [] in
      FunDef(export, BaseTy(rettype), name, params, blocks)
    end
  | _ -> failwith "expected 'function'"

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

(* let () = *)
(*   let ls = open_stream "mandel.ssa" in *)
(*   ignore (Printf.printf "%s" (dump (parse_function ls true))) *)
