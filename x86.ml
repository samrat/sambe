open Qbe_parser
open Printf
open ExtLib

type reg =
  | RAX
  | RBX
  | RCX
  | RDX
  | RSP
  | RBP
  | RDI
  | RSI

  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

type bytereg =
  | AL

type size =
  | QWORD_PTR
  | DWORD_PTR
  | WORD_PTR
  | BYTE_PTR

type arg =
  | Const of int
  | HexConst of int
  | Reg of reg
  | ByteReg of bytereg
  | RegOffset of int * reg
  | Sized of size * arg
  (* pseudo-arg for before reg. allocation *)
  | Var of string

type cc =
  | E | NE | LE | LT | GT | GE
  | A | AE | B | BE

type instruction =
  | IMov of arg * arg
  | IMovZx of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | ICmp of arg * arg

  | ISet of cc * arg
            
  | IJne of string
  | IJe of string
  | IJmp of string
  | ICall of string
  | IRet

  | IPush of arg
  | IPop of arg

  | ILabel of string


let arg_reg_order = List.map (fun r -> Reg(r))
    [RDI; RSI; RDX; RCX; R8; R9]

let caller_save_regs = List.map (fun r -> Reg(r))
    [RDX; RCX; RSI; RDI; R8; R9; R10; R11]

let callee_save_regs = List.map (fun r -> Reg(r))
    [RBX; R12; R13; R14; R15]

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
      | "sub" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          ISub(Reg(RAX), get_arg_val arg2) ]
        
      (* TODO: There is a lot of repetetive code for the compare
         instructions below. It should be simple to factor out all
         common code. *)
      | "ceqw" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(E, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL)) ]
      | "ceql" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(E, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]

      | "cnew" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(NE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
      | "cnel" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(NE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]

      | "cslew" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(LE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
      | "cslel" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(LE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]

      | "csltw" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(LT, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
      | "csltl" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(LT, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]

      | "csgew" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(GE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
      | "csgel" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(GE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]

      | "csgtw" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(GT, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
      | "csgtl" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(GT, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]

      | "culew" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(BE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
      | "culel" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(BE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]

      | "cultw" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(B, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
      | "cultl" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(B, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]

      | "cugew" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(AE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
      | "cugel" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(AE, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]

      | "cugtw" ->
        [ ICmp(Sized(DWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(A, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
      | "cugtl" ->
        [ ICmp(Sized(QWORD_PTR, get_arg_val arg1), get_arg_val arg2);
          ISet(A, ByteReg(AL));
          IMovZx(Reg(RAX), ByteReg(AL))]
    
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
  | Call(GlobalIdent(fn), args) ->
    (* save caller-save registers *)
    (List.map (fun r -> IPush(r)) caller_save_regs) @
    (* TODO: handle more than 6 args *)
    (List.map2 (fun (ty, arg) reg ->
        IMov(reg, get_arg_val arg))
        (List.take 6 (List.rev args))
        (List.take (min 6 (List.length args)) arg_reg_order)) @
    [ ICall(fn) ] @
    (* Restore caller-save registers *)
    (List.map (fun r -> IPop(r)) (List.rev caller_save_regs))
  | _ -> failwith "NYI: instr_to_x86"

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
  | Sized(size, Var(dest)), src ->
    let dest_loc = find_loc dest in
    (Sized(size, dest_loc), src)
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
  | RBX -> "rbx"
  | RCX -> "rcx"
  | RDX -> "rdx"
  | RSP -> "rsp"
  | RBP -> "rbp"
  | RDI -> "rdi"
  | RSI -> "rsi"
  | R8  -> "r8"
  | R9  -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

let br_to_asm = function
  | AL -> "al"

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
  | ByteReg(br) -> br_to_asm br
  | RegOffset(n, r) ->
    if n >= 0 then
      sprintf "[%s+%d]" (r_to_asm r) n
    else
      sprintf "[%s-%d]" (r_to_asm r) (-1 * n)
  | Sized(s, a) ->
    sprintf "%s %s" (s_to_asm s) (arg_to_asm a)
  | _ -> failwith (sprintf "NYI: %s" (ExtLib.dump a))

let cc_to_asm = function
  | LE -> "le"
  | E -> "e"
  | NE -> "ne"
  | LT -> "l"
  | GT -> "g"
  | GE -> "ge"
  | A -> "a"
  | AE -> "ae"
  | B -> "b"
  | BE -> "be"
    
let i_to_asm (i : instruction) : string =
  match i with
    | IMov(dest, value) ->
      sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
    | IMovZx(dest, src) ->
      sprintf "  movzx %s, %s" (arg_to_asm dest) (arg_to_asm src) 
    | IAdd(dest, to_add) ->
      sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
    | ISub(dest, to_sub) ->
      sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
    | IMul(dest, to_mul) ->
      sprintf "  mul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
    | ICmp(left, right) ->
      sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ILabel(name) ->
      sprintf "%s:" name
    | ISet(cond, dest) ->
      sprintf "  set%s %s" (cc_to_asm cond) (arg_to_asm dest)
    | IJe(label) ->
      sprintf "  je %s" label
    | IJne(label) ->
      sprintf "  jne %s" label
    | IJmp(arg) ->
      sprintf "  jmp %s" arg
    | ICall(label) ->
      sprintf "  call %s" label
    | IPush(arg) ->
      sprintf "  push %s" (arg_to_asm arg)
    | IPop(arg) ->
      sprintf "  pop %s" (arg_to_asm arg)
    | IRet ->
      let restore_callee_save_regs = (List.fold_left (fun acc r ->
          let r = match r with
            | Reg(reg) -> reg
            | _ -> failwith "expected Reg"
          in
          acc ^ (sprintf "  pop %s\n" (r_to_asm r)))
          ""
          (List.rev callee_save_regs))
      in
      sprintf "%s
  mov rsp, rbp
  pop rbp
  ret" restore_callee_save_regs


let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is

let asm_of_func (func : qbe) : string =
  match func with
  | FunDef(_, _, name, args, blocks) ->
    let mappings : (string, arg) Hashtbl.t = Hashtbl.create 16 in
    (* TODO: Handle more than 6 args *)
    ignore (List.map2 (fun (ty, arg) reg ->
        Hashtbl.add mappings (get_ident_name arg) reg)
        (List.take 6 (List.rev args))
        (List.take (min 6 (List.length args)) arg_reg_order));
    let (num_vars, compiled_blocks) =
      blocks
      |> Cfg.uniquify_block_labels (get_ident_name name)
      |> List.map block_to_x86
      |> List.flatten
      |> (fun x -> assign_homes x mappings)
    in
    let save_callee_save_regs = (List.fold_left (fun acc r ->
        let r = match r with
          | Reg(reg) -> reg
          | _ -> failwith "expected Reg"
        in
        acc ^ (sprintf "  push %s\n" (r_to_asm r)))
        ""
        callee_save_regs)
    in
    Printf.sprintf "section .text
global foo
%s:
  push rbp
  mov rbp, rsp
  sub rsp, %d
%s
  jmp %s
%s"
      (get_ident_name name)
      (8*num_vars)
      save_callee_save_regs
      (get_ident_name name ^ "_start")
      (to_asm compiled_blocks)
  | _ -> failwith "NYI"

let compile_to_file func_string =
  let func = Qbe_lexer.stream_of_string func_string in
  let parsed_func = Qbe_parser.parse_function func true in
  let dessad = Cfg.de_ssa parsed_func in
  let compiled = asm_of_func dessad in

  let oc = open_out "test.s" in
  fprintf oc "%s" compiled;
  close_out oc

(*

print_endline (to_asm (assign_homes (block_to_x86 (Block (BlockLabel "ift", [],
     [Assign (FuncIdent "y", BaseTy W, Instr1 ("copy", Integer 1)); Assign (FuncIdent "z", BaseTy W, Instr1 ("copy", Integer 1)); Assign (FuncIdent "idx2", BaseTy L,
     Instr2 ("add", FuncIdent "idx1", FuncIdent "arr"))],
     Instr1 ("jmp", BlockLabel "retstmt"))))))

*)
