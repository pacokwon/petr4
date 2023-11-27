open Poulet4
open AST
open ToP4cub
open Cimpl
open Core
open Result.Let_syntax

type ctx = coq_DeclCtx

type error =
  | NotFound of string
  | V1Model of string
  | Unsupported of string
      
let string_of_error = function
  | NotFound msg -> msg ^ ": not found"
  | V1Model msg -> msg ^ ": not well-formed for V1Model"
  | Unsupported msg -> msg ^ ": unsupported"

type 'a result = ('a,error) Core.result 

let name_of_top (t : Top.t) : string =
  match t with
  | Instantiate (_,n,_,_,_) -> n
  | Extern (n,_,_,_,_) -> n
  | Control (n,_,_,_,_,_,_) -> n
  | Parser (n,_,_,_,_,_,_) -> n
  | Funct (n,_,_,_) -> n

let lookup (l:Top.t list) (n:string) : Top.t result =
  let f t = String.equal (name_of_top t) n in
  match List.find l ~f with
  | Some c ->
    Ok c
  | None ->
    Error (NotFound n)

module Debug = struct
  let string_of_ctx ctx =
    let string_of_tops ts = String.concat ~sep:",\n  " (List.map ts ~f:name_of_top) in 
    let string_of_types ts = String.concat ~sep:",\n  " (List.map ts ~f:(fun (s,_) -> s)) in 
    Printf.sprintf "{ packages = \n  %s\nparsers = \n  %s\ncontrols = \n  %s\nexterns = \n  %s\ntypes = \n  %s\n}"
      (string_of_tops ctx.packages)
      (string_of_tops ctx.parsers)
      (string_of_tops ctx.controls)
      (string_of_tops ctx.externs)
      (string_of_types ctx.types)
    
  let rec string_of_type (t:Typ.t) =
    match t with
    | Bool -> "bool"
    | VarBit n -> Printf.sprintf "varbit<%s>" (Bigint.to_string n)
    | Bit n -> Printf.sprintf "bit<%s>" (Bigint.to_string n)
    | Int n -> Printf.sprintf "int<%s>" (Bigint.to_string n)
    | Error -> "error"
    | Array(_,t1) ->
       "array[" ^ string_of_type t1 ^ "]"
    | Struct(b,ts) ->
       let ss = List.map ts ~f:string_of_type in
       let s = String.concat ~sep:"," ss in
       if b then      
         Printf.sprintf "header { %s }" s
       else
         Printf.sprintf "struct { %s }" s     
    | Var n -> Printf.sprintf "X_%d" n
end

type tenv = Typ.t -> (ctyp * string list) result
    
let init_te t = Error (NotFound ("no binding in type environment: " ^ Debug.string_of_type t))

let update_te te t ct fs : tenv = (function t' -> if Poly.(t = t') then Ok (ct,fs) else te t')


let lookup_package prog = lookup prog.packages
let lookup_instance prog = lookup prog.packages
let lookup_control prog = lookup prog.controls
let lookup_parser prog = lookup prog.parsers
 
let fetch_v1model_instances (ctx:ctx) =
  match%bind lookup_package ctx "main" with
  | Instantiate(_,_,type_args,[p;v;i;e;u;d],_) ->
    List.iter type_args ~f:(fun t -> Printf.printf "TYPE: %s\n" (Debug.string_of_type t));
    Ok (p,v,i,e,u,d)
  | _ ->
    Error (V1Model "'main'")

let compile_z te z : int =
  Bigint.to_int_exn z

let compile_type (te:tenv) (t:Typ.t) : ctyp result =
  match te t with
  | Ok (t,_) -> Ok t
  | Error _ -> 
    match t with 
    | Bool ->
      Ok CTChar
    | VarBit(n) ->
      Error (V1Model ("varbits unsupported"))
    | Bit(z) ->
      let n = Bigint.to_int_exn z in
      if n <= 32 then Ok CTUInt
      else if n <= 64 then Ok CTULInt
      else Error(V1Model ("unsupported bit width " ^ string_of_int n))
    | Int(z) ->
      let n = Bigint.to_int_exn z in
      if n <= 32 then Ok CTInt
      else if n <= 64 then Ok CTLInt
      else Error(V1Model ("unsupported int width " ^ string_of_int n))
    | Error ->
      Ok CTChar
    | Array(n,t) ->
      Error (V1Model "arrays unsupported")
    | Struct(h,fs) ->
      Error(V1Model "unexpected struct")
    | Var(x) -> Error (V1Model "unexpected type variable")
    
let compile_expr (te:tenv) (e:Exp.t) : cexpr result =
  match e with
  | Bool b -> Ok (CEBool b)
  | VarBit _ -> Error (Unsupported "varbit")
  | Bit(w,n) -> Ok (CEInt (compile_z te n))
  | Int(w,n) -> Ok (CEInt (compile_z te n))
  | Var (t,x,n) -> Ok (CEVar x)
  | Slice(hi,lo,e1) -> Error(Unsupported "slice")
  | Cast(t,e) -> Error(Unsupported "cast")
  | Uop _ -> Error(Unsupported "unary operation")
  | Bop _ -> Error(Unsupported "binary operation")
  | Lists _ -> Error(Unsupported "lists")
  | Index _ -> Error(Unsupported "index")
  | Member(_,n,Var(t,x,_)) ->
      let%bind _,fs = te t in
      let f = List.nth_exn fs n in
      Ok (CEMember(CEVar x, f))
  | Member _ ->    Error(Unsupported "member")
  | Error _ -> Error(Unsupported "error")

let rec compile_statement (te:tenv) (s:Stm.t) : cblk result =
  match s with
  | Skip -> Ok ([])
  | Ret _ -> Error (Unsupported "return")
  | Exit -> Error (Unsupported "exit")
  | Trans(t) -> Error (V1Model "transition")
  | Asgn(lhs,rhs) ->
    let%bind l = match%bind compile_expr te lhs with
      | CEVar _ as l -> Ok l
      | CEMember _ as l -> Ok l
      | _ -> Error (V1Model "expected variable or member") in
    let%bind r = compile_expr te rhs in
    Ok ([CSAssign(l,r)])
  | SetValidity (b, hdr) -> Error (Unsupported "set validity")
  | App(f,args) -> Error(Unsupported "application")
  | Invoke(lhs,table) -> Error(Unsupported "table invocation")
  | LetIn(x,init,body) -> Error(Unsupported "let-in")
  | Seq(s1,s2) ->
    let%bind k1 = compile_statement te s1 in
    let%bind k2 = compile_statement te s2 in
    Ok (k1@k2)
  | Cond(b,then_,else_) -> Error(Unsupported "if-then-else")

let compile_param te (x,t) =
  let%bind ct = compile_type te t in
  Ok(ct, x)

let compile_params te ps =
  List.map ps ~f:(compile_param te) |>
  Result.all

let compile_control (te:tenv) (t:Top.t) (y:string) (args:Exp.t list) : cdecl result =
  match t with
  | Control (c, cparams, expr_params, eparams, params, body, apply) ->
    let%bind params = compile_params te params.inn in
    let%bind body = compile_statement te apply in
    Ok (CDFunction(CTVoid, y, params, body))
  | Instantiate _ -> Error (V1Model "expected control, found Instantiate")
  | Extern _ -> Error (V1Model "expected control, found Extern")
  | Parser _ -> Error (V1Model "expected control, found Parser")
  | Funct _ -> Error (V1Model "expected control, found Funct")

let fetch_parser (ctx:ctx) (p:string) : (string * Top.t) result =
  match lookup_parser ctx p with
  | Ok Instantiate(x,_,_,_,_) ->
    let%bind parser = lookup_parser ctx x in
    Ok (x,parser)
  | _ ->
    Error (V1Model "expected instantiate")

let fetch_control (ctx:ctx) (c:string) : (string * Top.t) result =
  match lookup_control ctx c with
  | Ok Instantiate(x,_,_,_,_) ->
    let%bind control = lookup_control ctx x in
    Ok (x,control)
  | _ ->
    Error (V1Model "expected instantiate")

let compile_parser (te:tenv) (t:Top.t) (y:string) (args:Exp.t list) : cdecl result =
  Ok (CDFunction(CTVoid, y, [], []))

let fetch_and_compile_control (te:tenv) ctx x =
  let%bind p = fetch_control ctx x in
  let y,control = p in
  let%bind cdecl = compile_control te control y [] in
  Ok (cdecl, CSCall(y,[]))

let fetch_and_compile_parser (te:tenv) ctx x =
  let%bind y,parser = fetch_parser ctx x in
  let%bind cdecl = compile_parser te parser y [] in
  Ok (cdecl, CSCall(y,[]))

let std_meta_fields =
  let open Typ in
  [ (Bit(Bigint.of_int 9), "ingress_port");
    (Bit(Bigint.of_int 9), "egress_spec");
    (Bit(Bigint.of_int 9), "egress_port");
    (Bit(Bigint.of_int 32), "instance_type");
    (Bit(Bigint.of_int 32), "packet_length");
    (Bit(Bigint.of_int 32), "enq_timestamp");
    (Bit(Bigint.of_int 19), "enq_qdepth");
    (Bit(Bigint.of_int 32), "deq_timedelta");
    (Bit(Bigint.of_int 19), "deq_qdepth");
    (Bit(Bigint.of_int 48), "ingress_global_timestamp");
    (Bit(Bigint.of_int 48), "egress_global_timestamp");
    (Bit(Bigint.of_int 16), "mcast_grp");
    (Bit(Bigint.of_int 16), "egress_rid");
    (Bit(Bigint.of_int 1), "checksum_error");
    (Error, "parser_error");
    (Bit(Bigint.of_int 3), "priority") ]

let te_v1model =
  let te0 = init_te in
  let t1 = Typ.Struct(false, List.map ~f:fst std_meta_fields) in
  let fs1 = List.map ~f:snd std_meta_fields in
  let ct1 = CTStruct "standard_metadata_t" in
  let t2 = Typ.Struct(false, []) in
  let fs2 = [] in
  let ct2 = CTStruct "empty" in
  let t3 = Typ.Struct(false, [Typ.Bool]) in
  let fs3 = ["ok"] in
  let ct3 = CTStruct "flag" in
  update_te te0 t1 ct1 fs1 |>
  (fun te -> update_te te t2 ct2 fs2) |>
  (fun te -> update_te te t3 ct3 fs3)

let compile_program (ctx:ctx) : cprog result =
  Printf.printf "%s" (Debug.string_of_ctx ctx);
  let%bind p,v,i,e,u,d = fetch_v1model_instances ctx in
  let _ = ignore (p,v,i,e,u,d) in
  let te = te_v1model in
  let%bind pfun,pcall = fetch_and_compile_parser te ctx p in
  let%bind vfun,vcall = fetch_and_compile_control te ctx v in
  let%bind ifun,icall = fetch_and_compile_control te ctx i in
  let%bind efun,ecall = fetch_and_compile_control te ctx e in
  let%bind ufun,ucall = fetch_and_compile_control te ctx u in
  let%bind dfun,dcall = fetch_and_compile_control te ctx d in  
  let main_blk = [ pcall; vcall; icall; ecall; ucall; dcall ] in
  let main_fun = CDFunction(CTInt, "main", [], main_blk) in
  let empty = CDStruct("empty", []) in
  let std_meta_res = List.map std_meta_fields ~f:(fun (t,x) ->
      let%bind ct = compile_type te t in
      Ok (ct,x)) in
  let%bind std_meta_cts = Result.all std_meta_res in
  let std_meta = CDStruct("standard_metadata", std_meta_cts) in
  let flag_meta = CDStruct("flag", [(CTChar, "ok")]) in
  Ok (Cimpl.CProgram [std_meta;empty;flag_meta;pfun;vfun;ifun;efun;ufun;dfun;main_fun])
