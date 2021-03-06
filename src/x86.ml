open Qbe_parser
open Printf
open ExtLib

type dwordreg =                 (* 32-bit registers *)
  | EAX
  | EBX
  | ECX
  | EDX
  | ESP
  | EBP
  | EDI
  | ESI

type bytereg =
  | AL

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

type freg =
  | ST0 | ST1 | ST2 | ST3
  | ST4 | ST5 | ST6 | ST7

type ssereg =
  | Xmm0 | Xmm1 | Xmm2 | Xmm3 | Xmm4 | Xmm5 | Xmm6 | Xmm7
  | Xmm8 | Xmm9 | Xmm10 | Xmm11 | Xmm12 | Xmm13 | Xmm14 | Xmm15

type size =
  | QWORD_PTR
  | DWORD_PTR
  | WORD_PTR
  | BYTE_PTR

type arg =
  | Integer of Int64.t
  | Float of float
  | HexConst of int
  | Reg of reg
  | DWordReg of dwordreg
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
  | A | AE | B | BE | P | NP

type instruction =
  | IMov of arg * arg
  | IMovZx of arg * arg
  | IMovSx of arg * arg
  | IMovSd of arg * arg
  | IMovSs of arg * arg
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

  (* SSE instructions *)
  | IAddSs of arg * arg
  | IAddSd of arg * arg
  | ISubSs of arg * arg
  | ISubSd of arg * arg
  | IMulSs of arg * arg
  | IMulSd of arg * arg
  | IDivSs of arg * arg
  | IDivSd of arg * arg
  | IComiSs of arg * arg
  | IComiSd of arg * arg
  | ICvtSs2Sd of arg * arg
  | ICvtSd2Ss of arg * arg
  | ICvtSs2Si of arg * arg
  | ICvtSd2Si of arg * arg
  | ICvtSi2Ss of arg * arg
  | ICvtSi2Sd of arg * arg

let arg_reg_order = List.map (fun r -> Reg(r))
    [ RDI; RSI; RDX; RCX; R8; R9 ]

let sse_reg_order = List.map (fun r -> SSEReg(r))
    [ Xmm0; Xmm1; Xmm2; Xmm3; Xmm4; Xmm5; Xmm6; Xmm7 ]

let caller_save_regs = List.map (fun r -> Reg(r))
    [RDX; RCX; RSI; RDI; R8; R9; R10; R11]

(* NOTE: RBX is used as a scratch buffer *)
let callee_save_regs = List.map (fun r -> Reg(r))
    [R12; R13; R14; R15]

let get_ident_name = function
  | GlobalIdent(id) | FuncIdent(id) -> id
  | _ -> failwith "NYI"

let get_arg_val arg = match arg with
  | GlobalIdent(id) -> VarOffset(0, Var(id))
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

let rec cmp_instr_to_x86 size cmp (arg1, arg2) =
  let cmp_part = match size with
    | W -> [ ICmp(Sized(DWORD_PTR, get_arg_val arg1),
                  Sized(DWORD_PTR, get_arg_val arg2));]
    | L -> [ ICmp(Sized(QWORD_PTR, get_arg_val arg1),
                  get_arg_val arg2);]
    | S -> [ IMovSs(SSEReg(Xmm0), (get_arg_val arg1));
             IMovSs(SSEReg(Xmm1), (get_arg_val arg2));
             IComiSs(SSEReg(Xmm0), SSEReg(Xmm1)); ]
    | D -> [ IMovSd(SSEReg(Xmm0), (get_arg_val arg1));
             IMovSd(SSEReg(Xmm1), (get_arg_val arg2));
             IComiSd(SSEReg(Xmm0), SSEReg(Xmm1)); ]
    | _ -> failwith "expected integer or float type"
  in
  let set_result = [ ISet(cmp, ByteReg(AL));
                     IMovZx(Reg(RAX), ByteReg(AL)) ] in
  cmp_part @ set_result

let rec instr_to_x86 instr instr_ty =
  let size = match instr_ty with
    | Some(BaseTy(W)) | Some(BaseTy(S)) -> DWORD_PTR
    | Some(BaseTy(L)) | Some(BaseTy(D)) -> QWORD_PTR
    | None -> QWORD_PTR
    | _ -> failwith (sprintf "NYI: instr_to_x86 size %s" (ExtLib.dump instr_ty)) in
  match instr with
  | Assign(dest, typ, src_instr) ->
    let mov_to_dest = match typ with
    | BaseTy(S) -> [ IMovSs(Sized(DWORD_PTR, get_arg_val dest),
                            SSEReg(Xmm0)) ]
    | BaseTy(D) ->
      [ IMovSd(Sized(QWORD_PTR, get_arg_val dest),
               SSEReg(Xmm0)) ]
    | _ -> [ IMov(Sized(size, get_arg_val dest),
                  Sized(size, Reg(RAX))) ] in
    instr_to_x86 src_instr (Some(typ)) @ mov_to_dest

  | Instr1(op, arg) ->
    begin
      match op with
      | "copy" ->
        [ IMov(Sized(size, Reg(RAX)), get_arg_val arg) ]
      | "jmp" ->
        [ IJmp(get_label arg) ]
      | "ret" ->
        begin
        match instr_ty with
        | Some(BaseTy(S)) ->
          [ IMovSs(SSEReg(Xmm0), get_arg_val arg);
            IRet;
          ]
        | Some(BaseTy(D)) ->
          [ IMovSd(SSEReg(Xmm0), get_arg_val arg);
            IRet;
          ]
        | _ ->
          [ IMov(Sized(size, Reg(RAX)), get_arg_val arg);
            IRet ]
          end
      (* Allocate *)
      (* NOTE: TODO: when there are multiple alloc's in a block, this
         will be really space-inefficient. We need to figure out the
         total no. of bytes required in block and then align
         accordingly. *)
      | "alloc4" | "alloc8" | "alloc16" ->
        let num_bytes = (match get_arg_val arg with
            | Integer(i) -> i
            | _ -> failwith "expected constant") in
        let padding = Int64.(rem num_bytes (of_int 16)) in
        [ ISub(Reg(RSP), Integer(Int64.add num_bytes padding));
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
      (* Conversions *)
      | "extsw" ->
        let addr = get_arg_val arg in
        [
          IMovSx(Reg(RAX), addr);
        ]
      | "extuw" ->
        let addr = get_arg_val arg in
        [
          IMov(Sized(DWORD_PTR, Reg(RAX)),
               Sized(DWORD_PTR, addr));
        ]
      | "extsh"| "extuh" | "extsb" | "extub" -> failwith "NYI"
      | "exts" ->
        [
          ICvtSs2Sd(SSEReg(Xmm0), get_arg_val arg);
        ]
      | "truncd" ->
        [
          ICvtSd2Ss(SSEReg(Xmm0), get_arg_val arg);
        ]
      | "stosi" ->
        [
          ICvtSs2Si(Sized(size, Reg(RAX)), get_arg_val arg);
        ]
      | "dtosi" ->
        [
          ICvtSd2Si(Sized(size, Reg(RAX)), get_arg_val arg);
        ]
      | "swtof" | "sltof" ->
        begin
          match instr_ty with
          | Some(BaseTy(S)) -> [
              ICvtSi2Ss(SSEReg(Xmm0), get_arg_val arg);
            ]
          | Some(BaseTy(D)) -> [
              ICvtSi2Sd(SSEReg(Xmm0), get_arg_val arg);
            ]
          | _ -> failwith "swtof: expected single or double precision float"
        end
      | _ -> failwith "NYI"
    end
  | Instr2(op, arg1, arg2) ->
    begin
      match op with
      (* Arithmetic/logic *)
      | "add" ->
        begin
          match instr_ty with
          | Some(BaseTy(W)) | Some(BaseTy(L)) ->
            [ IMov(Reg(RAX), get_arg_val arg1);
              IAdd(Reg(RAX), get_arg_val arg2); ]
          | Some(BaseTy(D)) ->
            [ IMovSd(SSEReg(Xmm0), get_arg_val arg1);
              IMovSd(SSEReg(Xmm1), get_arg_val arg2);
              IAddSd(SSEReg(Xmm0), SSEReg(Xmm1));
            ]
          | Some(BaseTy(S)) ->
            [ IMovSs(SSEReg(Xmm0), get_arg_val arg1);
              IMovSs(SSEReg(Xmm1), get_arg_val arg2);
              IAddSs(SSEReg(Xmm0), SSEReg(Xmm1));
            ]
          | _ -> failwith "NYI"
        end
      | "sub" ->
        begin
          match instr_ty with
          | Some(BaseTy(W)) | Some(BaseTy(L)) ->
            [ IMov(Reg(RAX), get_arg_val arg1);
              ISub(Reg(RAX), get_arg_val arg2); ]
          | Some(BaseTy(D)) ->
            [ IMovSd(SSEReg(Xmm0), get_arg_val arg1);
              IMovSd(SSEReg(Xmm1), get_arg_val arg2);
              ISubSd(SSEReg(Xmm0), SSEReg(Xmm1));
            ]
          | Some(BaseTy(S)) ->
            [ IMovSs(SSEReg(Xmm0), get_arg_val arg1);
              IMovSs(SSEReg(Xmm1), get_arg_val arg2);
              ISubSs(SSEReg(Xmm0), SSEReg(Xmm1));
            ]
          | _ -> failwith "NYI"
        end
      | "mul" ->
        begin
          match instr_ty with
          | Some(BaseTy(W)) | Some(BaseTy(L)) ->
            [ IMov(Reg(RAX), get_arg_val arg1);
              IMul(Reg(RAX), get_arg_val arg2); ]
          | Some(BaseTy(D)) ->
            [ IMovSd(SSEReg(Xmm0), get_arg_val arg1);
              IMovSd(SSEReg(Xmm1), get_arg_val arg2);
              IMulSd(SSEReg(Xmm0), SSEReg(Xmm1));
            ]
          | Some(BaseTy(S)) ->
            [ IMovSs(SSEReg(Xmm0), get_arg_val arg1);
              IMovSs(SSEReg(Xmm1), get_arg_val arg2);
              IMulSs(SSEReg(Xmm0), SSEReg(Xmm1));
            ]
          | _ -> failwith "NYI"
        end
      | "div" ->
        begin
          match instr_ty with
          | Some(BaseTy(W)) | Some(BaseTy(L)) ->
            [ IMov(Reg(RAX), get_arg_val arg1);
              IMov(Reg(RBX), get_arg_val arg2);
              ICdq;
              ISDiv(Reg(RBX)); ]
          | Some(BaseTy(D)) ->
            [ IMovSd(SSEReg(Xmm0), get_arg_val arg1);
              IMovSd(SSEReg(Xmm1), get_arg_val arg2);
              IDivSd(SSEReg(Xmm0), SSEReg(Xmm1));
            ]
          | Some(BaseTy(S)) ->
            [ IMovSs(SSEReg(Xmm0), get_arg_val arg1);
              IMovSs(SSEReg(Xmm1), get_arg_val arg2);
              IDivSs(SSEReg(Xmm0), SSEReg(Xmm1));
            ]
          | _ -> failwith "NYI"
        end

      (* TODO: implement floating-point rem *)
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

      | "ceqw" ->
        cmp_instr_to_x86 W E (arg1, arg2)
      | "ceql" ->
        cmp_instr_to_x86 L E (arg1, arg2)
      | "ceqs" ->
        cmp_instr_to_x86 S E (arg1, arg2)
      | "ceqd" ->
        cmp_instr_to_x86 D E (arg1, arg2)

      | "cnew" ->
        cmp_instr_to_x86 W NE (arg1, arg2)
      | "cnel" ->
        cmp_instr_to_x86 L NE (arg1, arg2)
      | "cnes" ->
        cmp_instr_to_x86 S NE (arg1, arg2)
      | "cned" ->
        cmp_instr_to_x86 D NE (arg1, arg2)

      | "cslew" ->
        cmp_instr_to_x86 W LE (arg1, arg2)
      | "cslel" ->
        cmp_instr_to_x86 L LE (arg1, arg2)
      | "cles" ->
        cmp_instr_to_x86 S BE (arg1, arg2)
      | "cled" ->
        cmp_instr_to_x86 D BE (arg1, arg2)

      | "csltw" ->
        cmp_instr_to_x86 W LT (arg1, arg2)
      | "csltl" ->
        cmp_instr_to_x86 L LT (arg1, arg2)
      | "clts" ->
        cmp_instr_to_x86 S B (arg1, arg2)
      | "cltd" ->
        cmp_instr_to_x86 D B (arg1, arg2)

      | "csgew" ->
        cmp_instr_to_x86 W GE (arg1, arg2)
      | "csgel" ->
        cmp_instr_to_x86 L GE (arg1, arg2)
      | "cges" ->
        cmp_instr_to_x86 S AE (arg1, arg2)
      | "cged" ->
        cmp_instr_to_x86 D AE (arg1, arg2)

      | "csgtw" ->
        cmp_instr_to_x86 W GT (arg1, arg2)
      | "csgtl" ->
        cmp_instr_to_x86 L GT (arg1, arg2)
      | "cgts" ->
        cmp_instr_to_x86 S A (arg1, arg2)
      | "cgtd" ->
        cmp_instr_to_x86 D A (arg1, arg2)

      | "culew" ->
        cmp_instr_to_x86 W BE (arg1, arg2)
      | "culel" ->
        cmp_instr_to_x86 L BE (arg1, arg2)

      | "cultw" ->
        cmp_instr_to_x86 W B (arg1, arg2)
      | "cultl" ->
        cmp_instr_to_x86 L BE (arg1, arg2)

      | "cugew" ->
        cmp_instr_to_x86 W AE (arg1, arg2)
      | "cugel" ->
        cmp_instr_to_x86 L AE (arg1, arg2)
        
      | "cugtw" ->
        cmp_instr_to_x86 W A (arg1, arg2)
      | "cugtl" ->
        cmp_instr_to_x86 L A (arg1, arg2)

      | "cos" -> cmp_instr_to_x86 S NP (arg1, arg2)
      | "cod" -> cmp_instr_to_x86 D NP (arg1, arg2)

      | "cuos" -> cmp_instr_to_x86 S P (arg1, arg2)
      | "cuod" -> cmp_instr_to_x86 D P (arg1, arg2)

      (* Store *)
      | "storew" ->
        let v = get_arg_val arg1 in
        let dest = get_arg_val arg2 in
        [ IMov(Reg(RAX), dest);
          IMov(Sized(QWORD_PTR, RegOffset(0, RAX)), v) ]
      | "storel" -> failwith "NYI: storel"
      | "stores" ->
        let v = get_arg_val arg1 in
        let dest = get_arg_val arg2 in
        [ IMov(Reg(RAX), dest);
          IMov(Sized(QWORD_PTR, RegOffset(0, RAX)), v)
        ]
      | _ -> failwith (Printf.sprintf "NYI: instr_to_x86 %s" op)
    end
  | Instr3(op, arg1, arg2, arg3) ->
    begin
      match op with
      | "jnz" ->
        [ IMov(Reg(RAX), get_arg_val arg1);
          ICmp(Reg(RAX), Integer(Int64.zero));
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
    let (movs, _, _) = (List.fold_left (fun (movs, gen_regs, sse_regs) (ty, arg)  ->
         match ty with
         | BaseTy(D) | BaseTy(S) ->
           let dest = List.hd sse_regs in
           ((IMov(dest, get_arg_val arg))::movs, gen_regs, List.tl sse_regs)
         | _ ->
           let dest = List.hd gen_regs in
           ((IMov(dest, get_arg_val arg))::movs, List.tl gen_regs, sse_regs))
        ([], arg_reg_order, sse_reg_order)
        (List.rev args)) in
    movs @
    [ ICall(fn) ] @
    (* Restore caller-save registers *)
    (List.map (fun r -> IPop(r)) (List.rev caller_save_regs))
  | _ -> failwith "NYI: instr_to_x86"


let block_to_x86 (retty: ty option) block =
  match block with
  | Block(label, [], reg_instrs, last_instr) ->
    [ ILabel(get_label label) ] @
    List.flatten (List.map (fun i -> instr_to_x86 i None) reg_instrs) @
    (instr_to_x86 last_instr retty)
  | Block(_, phis, _, _) -> failwith "expected empty phi instr list"
  | _ -> failwith "NYI"

let rec hoist_single_instr_float instr counter new_data =
  begin
    match instr with
    | Assign(dest, typ, assign_instr) ->
      (* NOTE: TODO: We could check `typ` to figure out whether we
         should recurse at all. *)
      Assign(dest, typ, hoist_single_instr_float assign_instr counter new_data)
    | Instr1(op, arg) -> begin
        match arg with
        | Float(f) | Double(f) ->
          let var_name = sprintf "fp%d" !counter in
          (counter := !counter + 1);
          Hashtbl.add new_data arg var_name;
          Instr1(op, GlobalIdent(var_name))
        | _ -> instr
      end
    | Instr2(op, arg1, arg2) -> begin
        match (arg1, arg2) with
        | Float(f1), Float(f2) | Double(f1), Double(f2) ->
          let new_f1 = sprintf "fp%d" !counter in
          (counter := !counter + 1);
          Hashtbl.add new_data arg1 new_f1;
          let new_f2 = sprintf "fp%d" !counter in
          (counter := !counter + 1);
          Hashtbl.add new_data arg2 new_f2;
          Instr2(op, GlobalIdent(new_f1), GlobalIdent(new_f2))
        | Float(f1) , arg2 | Double(f1), arg2 ->
          let new_f1 = sprintf "fp%d" !counter in
          (counter := !counter + 1);
          Hashtbl.add new_data arg1 new_f1;
          Instr2(op, GlobalIdent(new_f1), arg2)
        | arg1, Float(f2) | arg1, Double(f2) ->
          let new_f2 = sprintf "fp%d" !counter in
          (counter := !counter + 1);
          Hashtbl.add new_data arg2 new_f2;
          Instr2(op, arg1, GlobalIdent(new_f2))
        | _, _ -> instr
      end
    (* NOTE: there are no Instr3's that take a float *)
    | Instr3(op, arg1, arg2, arg3) -> instr
    | Phi(bs) -> failwith "NYI"
    | _ -> instr
  end

let hoist_out_floats (block_instrs: instr list) (counter: int ref) (new_data: (qbe, string) Hashtbl.t)
  : (instr list) =
  let new_instrs = List.map (fun i -> (hoist_single_instr_float i counter new_data))
      block_instrs in
  new_instrs

let hoist_block_floats block counter new_data =
  match block with
  | Block(label, phis, reg_instrs, last_instr) ->
    let new_phis = hoist_out_floats phis counter new_data in
    let new_reg_instrs = hoist_out_floats reg_instrs counter new_data in
    let new_last_instr = hoist_single_instr_float last_instr counter new_data in
    Block(label, new_phis, new_reg_instrs, new_last_instr)
  | _ -> failwith "expected block"

let hoist_func_floats func counter new_data =
  match func with
  | FunDef(export, retty, name, args, blocks) ->
    let new_blocks = List.map (fun block -> hoist_block_floats block counter new_data) blocks in
    FunDef(export, retty, name, args, new_blocks)
  | _ -> failwith "expected FunDef"

(* TODO: Currently, this puts everything in memory. Implement the
   register allocation pass to fix that. *)
let assign_homes (block: instruction list)
      (mappings : (string, arg) Hashtbl.t) =
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
  | (Sized(size, Var(dest)), Var(src)) ->
    let dest_loc = find_loc dest in
    let src_loc = find_loc src in
    (Sized(size, dest_loc), src_loc)
  | Sized(ds, Var(dest)), Sized(ss, Var(src)) ->
    let dest_loc = find_loc dest in
    let src_loc = find_loc src in
    (Sized(ds, dest_loc), Sized(ss, src_loc))
  | dest, Sized(ss, Var(src)) ->
    let src_loc = find_loc src in
    (dest, Sized(ss, src_loc))
    
  | Var(dest), src ->
    let dest_loc = find_loc dest in
    (dest_loc, src)
  | dest, Var(src) ->
    let src_loc = find_loc src in
    (dest, src_loc)
    
  | (Sized(size, Var(dest)), src) ->
    let dest_loc = find_loc dest in
    (Sized(size, dest_loc), src)
    
  | (dest, src) -> (dest, src)
  in
  let fixup_instr = function
    | IMovSs(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      IMovSs(new_dest, new_src)
    | IMovSd(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      IMovSd(new_dest, new_src)
    | IMov(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      IMov(new_dest, new_src)
    | IMovSx(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      IMovSx(new_dest, new_src)

    | ICvtSs2Sd(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      ICvtSs2Sd(new_dest, new_src)
    | ICvtSd2Ss(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      ICvtSd2Ss(new_dest, new_src)
    | ICvtSs2Si(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      ICvtSs2Si(new_dest, new_src)
    | ICvtSd2Si(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      ICvtSd2Si(new_dest, new_src)
    | ICvtSi2Ss(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      ICvtSi2Ss(new_dest, new_src)
    | ICvtSi2Sd(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      ICvtSi2Sd(new_dest, new_src)

    | IAdd(d, s) ->
      let (new_dest, new_src) = replace_instr2_args_with_locs (d, s) in
      IAdd(new_dest, new_src)
    | ICmp(left, right) ->
      let (new_left, new_right) = replace_instr2_args_with_locs (left, right) in
      ICmp(new_left, new_right)
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

let dr_to_asm (dr : dwordreg) : string =
  match dr with
  | EAX -> "eax"
  | EBX -> "ebx"
  | ECX -> "ecx"
  | EDX -> "edx"
  | ESP -> "esp"
  | EBP -> "ebp"
  | EDI -> "edi"
  | ESI -> "esi"


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
  | Xmm1 -> "xmm1"
  | Xmm2 -> "xmm2"
  | Xmm3 -> "xmm3"
  | Xmm4 -> "xmm4"
  | Xmm5 -> "xmm5"
  | Xmm6 -> "xmm6"
  | Xmm7 -> "xmm7"
  | Xmm8 -> "xmm8"
  | Xmm9 -> "xmm9"
  | Xmm10 -> "xmm10"
  | Xmm11 -> "xmm11"
  | Xmm12 -> "xmm12"
  | Xmm13 -> "xmm13"
  | Xmm14 -> "xmm14"
  | Xmm15 -> "xmm15"


let br_to_asm = function
  | AL -> "al"

let s_to_asm (s : size) : string =
  match s with
  | DWORD_PTR -> "DWORD"
  | QWORD_PTR -> "QWORD"
  | WORD_PTR -> "WORD"
  | BYTE_PTR -> "BYTE"

let reg_size_fix (s: size) (a : arg) : arg =
  match (s, a) with
  | DWORD_PTR, Reg(r) -> begin
      match r with
      | RAX -> DWordReg(EAX)
      | RBX -> DWordReg(EBX)
      | RCX -> DWordReg(ECX)
      | RDX -> DWordReg(EDX)
      | RSP -> DWordReg(ESP)
      | RBP -> DWordReg(EBP)
      | RDI -> DWordReg(EDI)
      | RSI -> DWordReg(ESI)
      | _ -> failwith (sprintf "Reg %s does not have a DWORD counterpart"
                         (r_to_asm r))
    end
  | _ -> a

let rec arg_to_asm (a : arg) : string =
  match a with
  | Integer(n) -> (Int64.to_string n)
  (* TODO: floats get hoisted up to data segment. So, this should be
     removed *)
  | Float(n) -> sprintf "%f" n
  | HexConst(n) -> sprintf "0x%X" n
  | Reg(r) -> r_to_asm r
  | DWordReg(dr) -> dr_to_asm dr
  | ByteReg(br) -> br_to_asm br
  | FReg(fr) -> fr_to_asm fr
  | SSEReg(sr) -> sr_to_asm sr
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
    sprintf "%s %s" (s_to_asm s) (arg_to_asm (reg_size_fix s a))
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
  | P -> "p"
  | NP -> "np"
    
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
    | IMovSs(dest, src) ->
      sprintf "  movss %s, %s" (arg_to_asm dest) (arg_to_asm src)
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

    | IAddSs(left, right) ->
      sprintf "  addss %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IAddSd(left, right) ->
      sprintf "  addsd %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ISubSs(left, right) ->
      sprintf "  subss %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ISubSd(left, right) ->
      sprintf "  subsd %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IMulSs(left, right) ->
      sprintf "  mulss %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IMulSd(left, right) ->
      sprintf "  mulsd %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IDivSs(left, right) ->
      sprintf "  divss %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IDivSd(left, right) ->
      sprintf "  divsd %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IComiSs(left, right) ->
      sprintf "  comiss %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IComiSd(left, right) ->
      sprintf "  comisd %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ICvtSs2Sd(left, right) ->
      sprintf "  cvtss2sd %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ICvtSd2Ss(left, right) ->
      sprintf "  cvtsd2ss %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ICvtSs2Si(left, right) ->
      sprintf "  cvtss2si %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ICvtSd2Si(left, right) ->
      sprintf "  cvtsd2si %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ICvtSi2Ss(left, right) ->
      sprintf "  cvtsi2ss %s, %s" (arg_to_asm left) (arg_to_asm right)
    | ICvtSi2Sd(left, right) ->
      sprintf "  cvtsi2sd %s, %s" (arg_to_asm left) (arg_to_asm right)


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
  | FunDef(_, retty, name, args, blocks) ->
    let mappings : (string, arg) Hashtbl.t = Hashtbl.create 16 in
    (* TODO: Handle more than 6 args. ie. spill to stack *)
    (* Add args to `mappings` *)
    ignore (List.fold_left (fun (gen_regs, sse_regs) (ty, arg)  ->
         match ty with
           | BaseTy(D) | BaseTy(S) ->
             begin
               let dest = List.hd sse_regs in
               Hashtbl.add mappings (get_ident_name arg) dest;
               (gen_regs, List.tl sse_regs)
             end
           | _ ->
             begin
               let size = match ty with
                 | BaseTy(W) -> DWORD_PTR
                 | BaseTy(L) -> QWORD_PTR
                 | _ -> failwith "NYI: asm_of_func add arg mappings" in
               let dest = Sized(size, List.hd gen_regs) in
               Hashtbl.add mappings (get_ident_name arg) dest;
               (List.tl gen_regs, sse_regs)
             end)
        (arg_reg_order, sse_reg_order)
        (List.rev args));
    (* Add data defs to `mappings` *)
    ignore (List.map (fun data_def ->
        Hashtbl.add mappings data_def (Var(data_def)))
        data_def_names);
    let (num_vars, compiled_blocks) =
      blocks
      |> Cfg.uniquify_block_labels (get_ident_name name)
      |> List.map (block_to_x86 (Some(retty)))
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
      | Integer(i) -> (Int64.to_string i) ^ ", "
      | Float(f) -> (sprintf "%f, " f)
      | Double(f) -> (sprintf "%f, " f)
      | _ -> failwith (sprintf "expected Integer, but got %s" (ExtLib.dump v))
    in
    let get_size_char = function
      | ExtTy(B) -> "b"
      | BaseTy(W) -> "w"        (* 32-bit *)
      | BaseTy(L) -> "q"        (* 64-bit *)
      | BaseTy(S) -> "d"        (* 32-bit float *)
      | BaseTy(D) -> "q"        (* 64-bit float *)

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

let hoist_all_floats funcs =
  let new_data = Hashtbl.create 12 in
  let counter = ref 0 in
  let new_funcs = List.map (fun func -> hoist_func_floats func counter new_data) funcs in
  let new_data_defs = Hashtbl.fold
      (fun k v acc ->
         let typ = match k with
           | Double(_) -> BaseTy(D)
           | Float(_) -> BaseTy(S)
           | _ -> failwith "expected float or double" in
         (DataDef(false, GlobalIdent(v), [(typ, [k])]))::acc)
      new_data [] in
  (new_funcs, new_data_defs)

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
  let (new_func_defs, additional_data_defs) = hoist_all_floats func_defs in
  let typechecked = List.map Typecheck.typecheck_func new_func_defs in

  let data_defs = data_defs @ additional_data_defs in
  let data_def_names = List.map (fun data_def -> match data_def with
      | DataDef(_, GlobalIdent(name), _) -> name
      | _ -> failwith "DataDef expected")
      data_defs in
  let compiled_data_defs = List.fold_left (fun acc x -> acc ^ (asm_of_data x)) "" data_defs in
  (* let compiled_type_defs = List.fold_left (fun acc x -> acc ^ (compile_toplevel x)) "" type_defs in *)
  let compiled_func_defs = List.fold_left (fun acc x -> acc ^ (asm_of_func x data_def_names)) "" new_func_defs in
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
