open Qbe_parser

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


let get_arg_val = function
  | FuncIdent(id) -> Var(id)
  | Integer(i) -> Const(i)
  | _ -> failwith "NYI"

let get_label = function
  | BlockLabel(label) -> label
  | _ -> failwith "expected BlockLabel"

let rec instr_to_x86 instr =
  match instr with
  | Assign(dest, ty, src_instr) ->
    instr_to_x86 src_instr @
    [ IMov(get_arg_val dest, Reg(RAX))]
  | Instr1(op, arg) ->
    begin
      match op with
      | "copy" -> 
        [ IMov(Reg(RAX), get_arg_val arg) ]
      | "jmp" ->
        [ IJmp(get_label arg) ]
      | _ -> failwith "NYI"
    end
  | Instr2(op, arg1, arg2) ->
    begin
      match op with
      | "add" -> 
        [ IMov(Reg(RAX), get_arg_val arg1);
          IAdd(Reg(RAX), get_arg_val arg2); ]
      | _ -> failwith "NYI"
    end
  | Instr3(op, arg1, arg2, arg3) ->
    begin
      match op with
      | "jnz" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          ICmp(Const(0), Reg(RAX));
          IJne(get_label arg3);
          (* TODO: Can we avoid this jump by generating code for the
             `arg2` block here? *)
          IJmp(get_label arg2);
        ]
      | _ -> failwith "NYI"
    end
    
  | _ -> failwith "NYI"

let block_to_x86 block =
  match block with
  | Block(label, [], reg_instrs, last_instr) ->
    [ ILabel(get_label label) ] @
    List.flatten (List.map instr_to_x86 reg_instrs) @
    (instr_to_x86 last_instr)
  | Block(_, phis, _, _) -> failwith "expected empty phi instr list"
  | _ -> failwith "NYI"


(* TODO: Currently, this puts everything in memory. Implement the
   register allocation pass to fix that. *)
let assign_homes (func: instruction list) =
  let mappings : (string, arg) Hashtbl.t = Hashtbl.create 16 in
  let counter = ref 0 in
  let find_loc v =
    match Hashtbl.find_option mappings v with
    | Some(loc) -> loc
    | None ->
      let new_loc = RegOffset(-8 * !counter, RBP) in
      Hashtbl.add mappings v new_loc;
      counter := !counter + 1;
      new_loc
  in
  let replace_instr2_args_with_locs = function
  | Var(dest), Var(src) ->
    let dest_loc = find_loc dest in
    let src_loc = find_loc src in
    (dest_loc, src_loc)
  | Var(dest), src ->
    let dest_loc = find_loc dest in
    (dest_loc, src)
  | dest, Var(src) ->
    let src_loc = find_loc src in
    (dest, src_loc)
  | dest, src ->
    (dest, src)
  in
  let fixup_instr = function
    | IMov(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      IMov(new_dest, new_src)
    | IAdd(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      IAdd(new_dest, new_src)
    | ICmp(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      ICmp(new_dest, new_src)
    | instr -> instr
  in
  List.map fixup_instr func
