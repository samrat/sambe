open Qbe_parser
open Printf
open ExtLib

type reg =
  | RAX
  | RSP
  | RBP
  | RDI
  | RSI
  | RDX
  | RCX
  | R8
  | R9

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
  | IMul of arg * arg
  | ICmp of arg * arg
            
  | IJne of string
  | IJmp of string
  | IRet

  | ILabel of string

let get_ident_name = function
  | GlobalIdent(id) | FuncIdent(id) -> id
  | _ -> failwith "NYI"

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
      | "ret" ->
        [ IMov(Reg(RAX), get_arg_val arg);
          IRet ]
      | _ -> failwith "NYI"
    end
  | Instr2(op, arg1, arg2) ->
    begin
      match op with
      | "add" -> 
        [ IMov(Reg(RAX), get_arg_val arg1);
          IAdd(Reg(RAX), get_arg_val arg2); ]
      | "mul" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          IMul(Reg(RAX), get_arg_val arg2); ]
      | "sub"
      | "cslew" ->
        [ ]                     (* TODO *)
      | _ -> failwith (Printf.sprintf "NYI: instr_to_x86 %s" op)
    end
  | Instr3(op, arg1, arg2, arg3) ->
    begin
      match op with
      | "jnz" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          ICmp(Reg(RAX), Const(0));
          IJne(get_label arg2);
          (* TODO: Can we avoid this jump by generating code for the
             `arg3` block here? *)
          IJmp(get_label arg3);
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
let assign_homes (block: instruction list) (mappings : (string, arg) Hashtbl.t)=
  let counter = ref 1 in
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
  (!counter - 1, List.map fixup_instr block)


let r_to_asm (r : reg) : string =
  match r with
    | RAX -> "rax"
    | RSP -> "rsp"
    | RBP -> "rbp"
    | RDI -> "rdi"
    | RSI -> "rsi"
    | RDX -> "rdx"
    | RCX -> "rcx"
    | R8  -> "r8"
    | R9  -> "r9"

let s_to_asm (s : size) : string =
  match s with
  | DWORD_PTR -> "DWORD"
  | QWORD_PTR -> "QWORD"
  | WORD_PTR -> "WORD"
  | BYTE_PTR -> "BYTE"

let rec arg_to_asm (a : arg) : string =
  match a with
  | Const(n) -> sprintf "%d" n
  | HexConst(n) -> sprintf "0x%X" n
  | Reg(r) -> r_to_asm r
  | RegOffset(n, r) ->
    if n >= 0 then
      sprintf "[%s+%d]" (r_to_asm r) n
    else
      sprintf "[%s-%d]" (r_to_asm r) (-1 * n)
  | Sized(s, a) ->
    sprintf "%s %s" (s_to_asm s) (arg_to_asm a)
  | _ -> failwith "NYI"

let i_to_asm (i : instruction) : string =
  match i with
    | IMov(dest, value) ->
      sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
    | IAdd(dest, to_add) ->
      sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
    | IMul(dest, to_mul) ->
      sprintf "  mul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
    | ICmp(left, right) ->
      sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ILabel(name) ->
      sprintf "%s:" name
    | IJne(label) ->
      sprintf "  jne %s" label
    | IJmp(arg) ->
      sprintf "  jmp %s" arg
    | IRet ->
      "  mov rsp, rbp
  pop rbp
  ret"

let arg_reg_order = List.map (fun r -> Reg(r))
    [RDI; RSI; RDX; RCX; R8; R9]

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is

let asm_of_func (func : qbe) : string =
  match func with
  | FunDef(_, _, name, args, blocks) ->
    let mappings : (string, arg) Hashtbl.t = Hashtbl.create 16 in
    (* TODO: Handle more than 6 args *)
    ignore (List.map2 (fun (ty, arg) reg ->
        Hashtbl.add mappings (get_ident_name arg) reg)
        (List.take 6 args)
        (List.take (min 6 (List.length args)) arg_reg_order));
    let (num_vars, compiled_blocks) =
      blocks
      |> Cfg.uniquify_block_labels (get_ident_name name)
      |> List.map block_to_x86
      |> List.flatten
      |> (fun x -> assign_homes x mappings)
    in
    Printf.sprintf "%s:
  push rbp
  mov rbp, rsp
  sub rsp, %d
  jmp %s
%s"
      (get_ident_name name)
      (8*num_vars)
      (name ^ "_start")
      (to_asm compiled_blocks)
  | _ -> failwith "NYI"

(*

print_endline (to_asm (assign_homes (block_to_x86 (Block (BlockLabel "ift", [],
     [Assign (FuncIdent "y", BaseTy W, Instr1 ("copy", Integer 1)); Assign (FuncIdent "z", BaseTy W, Instr1 ("copy", Integer 1)); Assign (FuncIdent "idx2", BaseTy L,
     Instr2 ("add", FuncIdent "idx1", FuncIdent "arr"))],
     Instr1 ("jmp", BlockLabel "retstmt"))))))

*)
