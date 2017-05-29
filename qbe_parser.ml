open Qbe_lexer
open Util
open ExtLib

(* TODO:
   - Phi currently assumes it will be passed two args.
   - Better errors(line numbers, exceptions- not just failwith)
   - error recovery
*)

type basety = S | D | W | L
            (* NOTE: Z appears only in data-defs. It specifies a
               size. For example: `z 100` would mean 100 zero-initialized
               bytes *)
            | Z
type extty =                    (* extended types *)
  | B | H

type ty = BaseTy of basety
        | ExtTy of extty
        | AggTy of string       (* aggregated type *)

type ident_ty = BlockIdent | AggType | FuncIdent | GlobalIdent


type instr = Assign of qbe * ty * instr (* dest * type * instr *)
           (* op arg1, arg2, arg3 *)
           | Instr3 of string * qbe * qbe * qbe
           | Instr2 of string * qbe * qbe   (* op arg1, arg2 *)
           | Instr1 of string * qbe         (* op arg1 *)
           | Instr0 of string
           (* used when last instruction of a block is not a
              jmp/return. This needs to be later replaced by a jump to
              the block that immediately follows the current one *)
           | JmpFixup
           | Phi of (qbe * qbe) list  (* phi  *)
           (* Function call *)
           | Call of qbe * (ty * qbe) list
and qbe =
  | String of string
  (* const *)
  | Integer of int
  (* TODO: handle different sizes *)
  | Float of float
  | Double of float
  | BlockLabel of string
  | AggType of string
  | FuncIdent of string
  | GlobalIdent of string

  (* for DataDef *)
  | IdentOffset of string * int

  (* typedef *)
  | TypeDef of
      qbe *                     (* name *)
      (ty * int) list *         (* fields *)
      int option                (* alignment *)

  (* datadef *)
  | DataDef of
      bool *                    (* export? *)
      qbe *                     (* name *)
      (ty * (qbe list)) list    (* dataitems *)

  | FunDef of
      bool *                    (* export? *)
      ty *                      (* return type *)
      qbe *                     (* name *)
      (ty * qbe) list *         (* params *)
      qbe list                  (* block list *)

  | Block of
      qbe *                     (* block label *)
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
  | _ -> failwith (Printf.sprintf "expected a keyword but instead got: %s" (dump kw))

let ident_of_string s =
  match explode s with
  | '@'::id -> BlockLabel(implode id)
  | ':'::id -> AggType(implode id)
  | '$'::id -> GlobalIdent(implode id)
  | '%'::id -> FuncIdent(implode id)
  | _ -> failwith (Printf.sprintf "not a valid identifier sigil: %s" (dump s))

let ident_of_token t =
  match t with
  | Ident(id) -> ident_of_string id
  | _ -> failwith "expected ident token"

let get_arg (ident : token) : qbe =
  match ident with
  | Ident(arg) -> ident_of_string arg
  | Integer(n) -> Integer(n)
  | Float(f) -> Float(f)
  | Double(d) -> Double(d)
  | _ -> failwith (Printf.sprintf "should have been an ident: %s" (dump ident))

let get_type (tystr : token) =
  match tystr with
  | Ident(ty) ->
    begin
      match ty with
      | "s" -> BaseTy(S)
      | "d" -> BaseTy(D)
      | "w" -> BaseTy(W)
      | "l" -> BaseTy(L)
      | "b" -> ExtTy(B)
      | "z" -> BaseTy(Z)
      | _ -> failwith (Printf.sprintf "not a type: %s" (dump ty))
    end
  | _ -> failwith "expected string"


let ident_type (id : qbe) =
  match id with
  | BlockLabel(_) -> BlockIdent
  | AggType(_) -> AggType
  | GlobalIdent(_) -> GlobalIdent
  | FuncIdent(_) -> FuncIdent
  | _ -> failwith "not an ident"

let get_params ls =
  let rec get_params' acc =
    match peek_token ls with
    | RParen -> ignore (next_token ls); List.rev acc
    | Ident(t) ->
      begin
        ignore (next_token ls);
        let ty = get_type (Ident(t)) in
        let param = get_arg (next_token ls) in
        let new_params = (ty, param) :: acc in
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


let parse_call ls =
  match peek_token ls with
  | Keyword("call") ->
    ignore (next_token ls);
    let fun_name = ident_of_token (next_token ls) in
    let params = get_params ls in
    Call(fun_name, params)
  | _ -> failwith "expected 'call'"


let parse_instruction ls =
  match peek_token ls with
  | Ident(dest) ->
    begin
      ignore (next_token ls);
      expect ls Equals;
      let rettype = next_token ls in
      let op = get_op (peek_token ls) in
      match op with
      | "phi" ->
        ignore (next_token ls);
        let src1 = get_arg (next_token ls) in
        let var1 = get_arg (next_token ls) in
        let _ = expect ls Comma in
        let src2 = get_arg (next_token ls) in
        let var2 = get_arg (next_token ls) in
        Assign((ident_of_string dest), get_type rettype,
               Phi([(src1, var1); (src2, var2)]))
      | "call" ->
        Assign((ident_of_string dest), get_type rettype,
               parse_call ls)
      | _ ->
        ignore (next_token ls);
        let arg1 = next_token ls in
        if peek_token ls = Comma
        then
          begin
            ignore (next_token ls);
            let arg2 = next_token ls in
            Assign((ident_of_string dest), get_type rettype,
                   Instr2(op, get_arg arg1, get_arg arg2))
          end
        else
          Assign((ident_of_string dest), get_type rettype,
                 Instr1(op, get_arg arg1))
    end
  | Keyword("call") ->
    parse_call ls
  | Keyword(op) ->
    begin
      ignore (next_token ls);
      if op = "ret" &&
         (match (peek_token ls) with
          | Ident(id) -> if ident_type (ident_of_string id) = BlockIdent
            then true else false
          | _ -> false)
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
  | Ident(id) ->
    if ident_type (ident_of_token (Ident(id))) = BlockIdent
    then Some 0 else None
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

      let last_instr = match remaining_block_size ls with
        | Some(0) ->
          JmpFixup
        | Some(1) -> parse_instruction ls
        | _ -> failwith "there should be at most one instruction between regular instructions and the next block"
      in
      Block((ident_of_string label), phis, instrs, last_instr)
    end
  | _ -> failwith "expected block label"


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
        let last_block = List.hd acc in
        let final_blocks = List.rev (List.tl acc) in
        (* TODO: check that last block isn't missing jump instr *)
        List.fold_right
          (fun block (next_block, acc) -> match block with
             | Block(label, phis, instrs, JmpFixup) ->
               let next_block_label = match next_block with
                 | Block(next_block_label, _, _, _) -> next_block_label
                 | _ -> failwith "expected block" in
               let new_block =
                 Block(label, phis, instrs,
                       Instr1("jmp", next_block_label)) in
               (new_block, new_block :: acc)
             | _ -> (block, block::acc))
          final_blocks
          (last_block, [last_block])
      end
    else parse_blocks ((parse_block ls) :: acc)
  in

  match peek_token ls with
  | Keyword("function") ->
    begin
      ignore (next_token ls);
      (* TODO: type can be either ident(aggregated type) or keyword *)
      (* TODO: rettype is optional *)
      let rettype = get_type (next_token ls) in
      let name = get_arg (next_token ls) in
      let params = get_params ls in
      let _ = expect ls LBrace in
      let (_, blocks) = parse_blocks [] in
      expect ls RBrace;
      FunDef(export, rettype, name, params, blocks)
    end
  | _ -> failwith "expected 'function'"


(* TODO: handle opaque types *)
let parse_typedef ls =
  let rec parse_items acc =
    match peek_token ls with
    | RBrace -> List.rev acc
    | Ident(ty) ->
      begin
        let item = (get_type (Ident(ty))) in
        ignore (next_token ls);
        let num = (match peek_token ls with
            | Integer(n) -> ignore (next_token ls); n
            | _ -> 1) in
        let newitems = ((item, num) :: acc) in
        if peek_token ls = Comma
        then begin
          ignore (next_token ls); 
          parse_items newitems
        end
        else List.rev newitems
      end
    | _ -> failwith "syntax error in typedef"
  in
  match peek_token ls with
  | Keyword("type") ->
    ignore (next_token ls);
    let type_name = get_arg (next_token ls) in
    (expect ls Equals);
    let alignment = (match peek_token ls with
        | Keyword("align") ->
          ignore (next_token ls);
          (match (next_token ls) with
           | Integer(i) -> Some(i)
           | _ -> failwith "expected integer")
        | _ -> None) in
    (expect ls LBrace);
    let items = parse_items [] in
    (expect ls RBrace);
    TypeDef(type_name, items, alignment)
  | _ -> failwith "expected 'type'"


let parse_datadef ls export =
  let rec parse_items acc =
    let rec parse_dataitems ds =
      match peek_token ls with
      | Comma | RBrace -> List.rev ds;
      | Integer(n) ->
        ignore (next_token ls);
        parse_dataitems (Integer(n)::ds)
      | Float(n) ->
        ignore (next_token ls);
        parse_dataitems (Float(n)::ds)
      | Double(n) ->
        ignore (next_token ls);
        parse_dataitems (Double(n)::ds)
      | String(s) ->
        ignore (next_token ls);
        parse_dataitems (String(s)::ds)
      | _ -> failwith "syntax error in dataitems"
    in
    match peek_token ls with
    | RBrace -> List.rev acc
    | Ident(ty) ->
      begin
        let ty = (get_type (Ident(ty))) in
        ignore (next_token ls);
        let dataitems = parse_dataitems [] in
        let newitems = ((ty, dataitems) :: acc) in
        if peek_token ls = Comma
        then begin
          ignore (next_token ls);
          parse_items newitems
        end
        else List.rev newitems
      end
    | _ -> failwith "syntax error in datadef"
  in
  match peek_token ls with
  | Keyword("data") ->
    ignore (next_token ls);
    let data_name = get_arg (next_token ls) in
    (expect ls Equals);
    (expect ls LBrace);
    let items = parse_items [] in
    (expect ls RBrace);
    DataDef(export, data_name, items)
  | _ -> failwith "expected 'data'"


let parse_func_or_data ls export =
  match peek_token ls with
  | Keyword("function") -> parse_function ls export
  | Keyword("data") -> parse_datadef ls export
  | _ -> failwith "expected 'function' or 'data'"

let parse_toplevel ls =
  match peek_token ls with
  | Keyword("export") ->
    ignore (next_token ls);
    parse_func_or_data ls true
  | Keyword("function") -> parse_function ls false
  | Keyword("data") -> parse_datadef ls false
  | Keyword("type") -> parse_typedef ls
  | _ -> failwith "not a valid top level def"

(* let () = *)
(*   let ls = open_stream "mandel.ssa" in *)
(*   ignore (Printf.printf "%s" (dump (parse_function ls true))) *)
