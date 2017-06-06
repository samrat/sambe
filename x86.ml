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

type freg =
  | ST0 | ST1 | ST2 | ST3
  | ST4 | ST5 | ST6 | ST7

type ssereg =
  | Xmm0

type size =
  | QWORD_PTR
  | DWORD_PTR
  | WORD_PTR
  | BYTE_PTR

type arg =
  | Integer of int
  | Float of float
  | HexConst of int
  | Reg of reg
  | ByteReg of bytereg
  | FReg of freg
  | SSEReg of ssereg
  | RegOffset of int * reg
  | VarOffset of int * arg
  | Sized of size * arg
  (* pseudo-arg for before reg. allocation *)
  | Var of string

type cc =
  | E | NE | LE | LT | GT | GE
  | A | AE | B | BE

type instruction =
  | IMov of arg * arg
  | IMovZx of arg * arg
  | IMovSx of arg * arg
  | IMovSd of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg
  | ISDiv of arg
  | IUDiv of arg
  | IShr of arg * arg
  | ISar of arg * arg
  | IShl of arg * arg
  | ICmp of arg * arg
  | ICdq

  | ISet of cc * arg
            
  | IJne of string
  | IJe of string
  | IJmp of string
  | ICall of string
  | IRet

  | IPush of arg
  | IPop of arg

  | ILabel of string

  | IFld of arg
  | IFst of arg
  | IFadd of arg * arg
  | IFsub of arg * arg
  | IFmul of arg * arg


let arg_reg_order = List.map (fun r -> Reg(r))
    [RDI; RSI; RDX; RCX; R8; R9]

let caller_save_regs = List.map (fun r -> Reg(r))
    [RDX; RCX; RSI; RDI; R8; R9; R10; R11]

(* NOTE: RBX is used as a scratch buffer *)
let callee_save_regs = List.map (fun r -> Reg(r))
    [R12; R13; R14; R15]

let get_ident_name = function
  | GlobalIdent(id) | FuncIdent(id) -> id
  | _ -> failwith "NYI"

let get_arg_val arg = match arg with
  | GlobalIdent(id) -> Var(id)
  | FuncIdent(id) -> Var(id)
  | Integer(i) -> Integer(i)
  | Float(f) -> Float(f)
  | Double(f) -> Float(f)
  | _ -> failwith (sprintf "NYI: get_arg_val %s" (ExtLib.dump arg))

let get_label = function
  | BlockLabel(label) -> label
  | _ -> failwith "expected BlockLabel"

let lookup x env =
  let rec lookup' = function
    | [] -> None
    | (v,typ) as l::ls -> if x = v then Some(l) else lookup' ls
  in
  lookup' env

let rec instr_to_x86 instr instr_ty =
  match instr with
  | Assign(dest, typ, src_instr) ->
    instr_to_x86 src_instr (Some(typ)) @
    [ IMov(get_arg_val dest, Reg(RAX)) ]
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
      (* Allocate *)
      (* NOTE: TODO: when there are multiple alloc's in a block, this
         will be really space-inefficient. We need to figure out the
         total no. of bytes required in block and then align
         accordingly. *)
      | "alloc4" | "alloc8" | "alloc16" ->
        let num_bytes = (match get_arg_val arg with
            | Integer(i) -> i
            | _ -> failwith "expected constant") in
        let padding = num_bytes mod 16 in
        [ ISub(Reg(RSP), Integer(num_bytes + padding));
          IMov(Reg(RAX), Reg(RSP)) ]

      (* Load *)
      | "loadw" ->
        let addr = get_arg_val arg in
        [ IMov(Reg(RAX), addr);
          IMov(Sized(DWORD_PTR, Reg(RAX)), RegOffset(0, RAX))]
      | "loadl" ->
        let addr = get_arg_val arg in
        [ IMov(Reg(RAX), addr);
          IMov(Sized(QWORD_PTR, Reg(RAX)), RegOffset(0, RAX))]
      | "loadsb" ->
        let addr = get_arg_val arg in
        [ IMov(Reg(RAX), addr);
          IMovSx(Reg(RAX), Sized(BYTE_PTR, RegOffset(0, RAX)))]
      | _ -> failwith "NYI"
    end
  | Instr2(op, arg1, arg2) ->
    begin
      match op with
      (* Arithmetic/logic *)
      (* TODO: perform only the necessary instruction here. Add
         another pass to move to the "scratch" register. *)
      | "add" ->
        begin
          match instr_ty with
          | Some(BaseTy(W)) | Some(BaseTy(L)) ->
            [ IMov(Reg(RAX), get_arg_val arg1);
              IAdd(Reg(RAX), get_arg_val arg2); ]
          | Some(BaseTy(D)) | Some(BaseTy(S)) ->
            [ IFld(Sized(QWORD_PTR, VarOffset(0, (get_arg_val arg1))));
              IFld(Sized(QWORD_PTR, VarOffset(0, (get_arg_val arg2))));
              IFadd(FReg(ST0), FReg(ST1));
              (* TODO: This `ret` shouldn't be a static string. (Will one
                 address in the data segment be enough?). Is there a
                 better way than to reserve space in the data/bss
                 segment? *)
              IFst(Sized(QWORD_PTR, VarOffset(0, Var("ret"))));
              IMovSd(SSEReg(Xmm0), VarOffset(0, Var("ret")));
            ]
          | _ -> failwith "NYI"
        end
      | "sub" ->
        begin
          match instr_ty with
          | Some(BaseTy(W)) | Some(BaseTy(L)) ->
            [ IMov(Reg(RAX), get_arg_val arg1);
              ISub(Reg(RAX), get_arg_val arg2); ]
          | Some(BaseTy(D)) | Some(BaseTy(S)) ->
            [ IFld(Sized(QWORD_PTR, VarOffset(0, (get_arg_val arg2))));
              IFld(Sized(QWORD_PTR, VarOffset(0, (get_arg_val arg1))));
              IFsub(FReg(ST0), FReg(ST1));
              (* TODO: This `ret` shouldn't be a static string. (Will one
                 address in the data segment be enough?). Is there a
                 better way than to reserve space in the data/bss
                 segment? *)
              IFst(Sized(QWORD_PTR, VarOffset(0, Var("ret"))));
              IMovSd(SSEReg(Xmm0), VarOffset(0, Var("ret")));
            ]
          | _ -> failwith "NYI"
        end
      | "mul" ->
        begin
          match instr_ty with
          | Some(BaseTy(W)) | Some(BaseTy(L)) ->
            [ IMov(Reg(RAX), get_arg_val arg1);
              IMul(Reg(RAX), get_arg_val arg2); ]
          | Some(BaseTy(D)) | Some(BaseTy(S)) ->
            [ IFld(Sized(QWORD_PTR, VarOffset(0, (get_arg_val arg2))));
              IFld(Sized(QWORD_PTR, VarOffset(0, (get_arg_val arg1))));
              IFmul(FReg(ST0), FReg(ST1));
              (* TODO: This `ret` shouldn't be a static string. (Will one
                 address in the data segment be enough?). Is there a
                 better way than to reserve space in the data/bss
                 segment? *)
              IFst(Sized(QWORD_PTR, VarOffset(0, Var("ret"))));
              IMovSd(SSEReg(Xmm0), VarOffset(0, Var("ret")));
            ]
          | _ -> failwith "NYI"
        end
      (* TODO: move arg2 to register *)
      | "div" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          IMov(Reg(RBX), get_arg_val arg2);
          ICdq;
          ISDiv(Reg(RBX)); ]
      | "rem" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          IMov(Reg(RBX), get_arg_val arg2);
          ICdq;
          ISDiv(Reg(RBX));
          IMov(Reg(RAX), Reg(RDX));]
      | "udiv" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          IMov(Reg(RBX), get_arg_val arg2);
          ICdq;
          IUDiv(Reg(RBX)); ]
      | "urem" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          IMov(Reg(RBX), get_arg_val arg2);
          ICdq;
          IUDiv(Reg(RBX));
          IMov(Reg(RAX), Reg(RDX));]
      | "sar" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          ISar(Reg(RAX), get_arg_val arg2) ]
      | "shr" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          ISar(Reg(RAX), get_arg_val arg2) ]
      | "shl" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          IShl(Reg(RAX), get_arg_val arg2) ]

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

      (* Store *)
      | "storew" ->
        let v = get_arg_val arg1 in
        let dest = get_arg_val arg2 in
        [ IMov(Reg(RAX), dest);
          IMov(Sized(QWORD_PTR, RegOffset(0, RAX)), v) ]
      | "storel" -> failwith "NYI: storel"

      | _ -> failwith (Printf.sprintf "NYI: instr_to_x86 %s" op)
    end
  | Instr3(op, arg1, arg2, arg3) ->
    begin
      match op with
      | "jnz" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          ICmp(Reg(RAX), Integer(0));
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
    List.flatten (List.map (fun i -> instr_to_x86 i None) reg_instrs) @
    (instr_to_x86 last_instr None)
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

let fr_to_asm (fr : freg) : string =
  match fr with
  | ST0 -> "st0"
  | ST1 -> "st1"
  | ST2 -> "st2"
  | ST3 -> "st3"
  | ST4 -> "st4"
  | ST5 -> "st5"
  | ST6 -> "st6"
  | ST7 -> "st7"

let sr_to_asm (sr : ssereg) : string =
  match sr with
  | Xmm0 -> "xmm0"


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
  | Integer(n) -> sprintf "%d" n
  (* TODO: floats get hoisted up to data segment. So, this should be
     removed *)
  | Float(n) -> sprintf "%f" n
  | HexConst(n) -> sprintf "0x%X" n
  | Reg(r) -> r_to_asm r
  | FReg(fr) -> fr_to_asm fr
  | SSEReg(sr) -> sr_to_asm sr
  | ByteReg(br) -> br_to_asm br
  | VarOffset(n, v) ->
    if n >= 0 then
      sprintf "[%s+%d]" (arg_to_asm v) n
    else
      sprintf "[%s-%d]" (arg_to_asm v) (-1 * n)
  | RegOffset(n, r) ->
    if n >= 0 then
      sprintf "[%s+%d]" (r_to_asm r) n
    else
      sprintf "[%s-%d]" (r_to_asm r) (-1 * n)
  | Sized(s, a) ->
    sprintf "%s %s" (s_to_asm s) (arg_to_asm a)
  | Var(v) ->
    sprintf "%s" v

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
    | IMovSx(dest, src) ->
      sprintf "  movsx %s, %s" (arg_to_asm dest) (arg_to_asm src)
    | IMovSd(dest, src) ->
      sprintf "  movsd %s, %s" (arg_to_asm dest) (arg_to_asm src)
    | IAdd(dest, to_add) ->
      sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
    | ISub(dest, to_sub) ->
      sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
    | IMul(dest, to_mul) ->
      sprintf "  mul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
    | ISDiv(denom) ->           (* numerator goes in RAX *)
      sprintf "  idiv %s" (arg_to_asm denom)
    | IUDiv(denom) ->           (* numerator goes in RAX *)
      sprintf "  div %s" (arg_to_asm denom)
    | IShr(dest, shift) ->
      sprintf "  shr %s, %s" (arg_to_asm dest) (arg_to_asm shift)
    | ISar(dest, shift) ->
      sprintf "  sar %s, %s" (arg_to_asm dest) (arg_to_asm shift)
    | IShl(dest, shift) ->
      sprintf "  shl %s, %s" (arg_to_asm dest) (arg_to_asm shift)
    | ICmp(left, right) ->
      sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ICdq -> "  cdq"
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
    | IFld(arg) ->
      sprintf "  fld %s" (arg_to_asm arg)
    | IFst(arg) ->
      sprintf "  fst %s" (arg_to_asm arg)
    | IFadd(left, right) ->
      sprintf "  fadd %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IFsub(left, right) ->
      sprintf "  fsub %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IFmul(left, right) ->
      sprintf "  fmul %s, %s" (arg_to_asm left) (arg_to_asm right)

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

let asm_of_func (func: qbe) (data_def_names: string list)  : string =
  match func with
  | FunDef(_, _, name, args, blocks) ->
    let mappings : (string, arg) Hashtbl.t = Hashtbl.create 16 in
    (* TODO: Handle more than 6 args *)
    (* Add args to `mappings` *)
    ignore (List.map2 (fun (ty, arg) reg ->
        Hashtbl.add mappings (get_ident_name arg) reg)
        (List.take 6 (List.rev args))
        (List.take (min 6 (List.length args)) arg_reg_order));
    (* Add data defs to `mappings` *)
    ignore (List.map (fun data_def ->
        Hashtbl.add mappings data_def (Var(data_def)))
        data_def_names);
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
    Printf.sprintf "\nglobal %s
%s:
  push rbp
  mov rbp, rsp
  sub rsp, %d
%s
  jmp %s
%s"
      (get_ident_name name)
      (get_ident_name name)
      (8*num_vars)
      save_callee_save_regs
      (get_ident_name name ^ "_start")
      (to_asm compiled_blocks)
  | _ -> failwith "expected function"


let asm_of_data = function
  | DataDef(export, name, fields) ->
    (* "ret: dq 0" *)
    let get_val_str (v : qbe) = match v with
      | Integer(i) -> (sprintf "%d, " i)
      | Float(f) -> (sprintf "%f, " f)
      | Double(f) -> (sprintf "%f, " f)
      | _ -> failwith (sprintf "expected Integer, but got %s" (ExtLib.dump v))
    in
    let get_size_char = function
      | ExtTy(B) -> "b"
      | BaseTy(W) -> "w"        (* 32-bit *)
      | BaseTy(L) -> "q"        (* 64-bit *)
      | BaseTy(D) -> "q"
      | BaseTy(S) -> "w"
      | _ -> failwith "NYI" in
    let field_vals_str (typ, vs) = List.fold_left 
        (fun acc x -> acc ^ (get_val_str x))
        (sprintf "d%s " (get_size_char typ))
        vs in
    let s = List.fold_left
        (fun acc x -> acc ^ "\n  " ^ x)
        ""
        (List.map field_vals_str fields) in
    (sprintf "\n%s: %s" (get_ident_name name) s)
  | _ -> failwith "expected data"


let segregate_toplevel_defs defs =
  List.fold_left (fun (data, typs, funcs) def ->
      match def with
      | FunDef(_, _, _, _, _) -> (data, typs, def::funcs)
      | DataDef(_, _, _) -> (def::data, typs, funcs)
      | TypeDef(_, _, _) -> (data, def::typs, funcs)
      | _ -> failwith "expected top level definition")
    ([], [], [])
    defs


let compile_to_file ls oc =
  let parsed_list = Qbe_parser.get_parsed_list ls in
  (* TODO: compose functions to avoid repeated maps *)
  let dessad = List.map Cfg.de_ssa parsed_list in
  let (data_defs, type_defs, func_defs) = 
    segregate_toplevel_defs dessad in
  let data_def_names = List.map (fun data_def -> match data_def with
      | DataDef(_, GlobalIdent(name), _) -> name
      | _ -> failwith "DataDef expected")
      data_defs in
  let compiled_data_defs = List.fold_left (fun acc x -> acc ^ (asm_of_data x)) "" data_defs in
  (* let compiled_type_defs = List.fold_left (fun acc x -> acc ^ (compile_toplevel x)) "" type_defs in *)
  let compiled_func_defs = List.fold_left (fun acc x -> acc ^ (asm_of_func x data_def_names)) "" func_defs in
  let compiled =
    "section .data" ^ compiled_data_defs ^
    (* TODO: type defs *)
    "\nsection .text" ^ compiled_func_defs in

  let oc = open_out "test.s" in
  fprintf oc "%s" compiled;
  close_out oc


let compile_str_to_file prog_string =
  let prog_stream = Qbe_lexer.stream_of_string prog_string in
  let outfile = "test.s" in
  let oc = open_out outfile in
  compile_to_file prog_stream oc


(*

print_endline (to_asm (assign_homes (block_to_x86 (Block (BlockLabel "ift", [],
     [Assign (FuncIdent "y", BaseTy W, Instr1 ("copy", Integer 1)); Assign (FuncIdent "z", BaseTy W, Instr1 ("copy", Integer 1)); Assign (FuncIdent "idx2", BaseTy L,
     Instr2 ("add", FuncIdent "idx1", FuncIdent "arr"))],
     Instr1 ("jmp", BlockLabel "retstmt"))))))

*)
