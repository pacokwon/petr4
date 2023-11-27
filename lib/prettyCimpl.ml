open Core
open Pp
open Pp.O

let semi = text ";"
let dot = text "."
   
let rec format_ctyp t =
  match t with
  | Cimpl.CTChar -> text "char"
  | Cimpl.CTVoid -> text "void"
  | Cimpl.CTInt -> text "int"
  | Cimpl.CTUInt -> text "unsigned int"
  | Cimpl.CTLInt -> text "long int"                      
  | Cimpl.CTULInt -> text "unsigned long int"                      
  | Cimpl.CTArray(t) -> format_ctyp t ++ text "[]"
  | Cimpl.CTStruct(s) -> text s

let format_cvar x =
  text x

let format_bop (x : Cimpl.bop) =
  match x with 
  | Cimpl.CBEq -> text "="
  | Cimpl.CBNe -> text "!="
  | Cimpl.CBLt -> text "<"
  | Cimpl.CBGt -> text ">"
  | Cimpl.CBGte -> text ">="
  | Cimpl.CBLte -> text "<="
  | Cimpl.CBAnd -> text "&&"
  | Cimpl.CBOr -> text "||"
  | Cimpl.CBAdd -> text "+"
  | Cimpl.CBSub -> text "-"
  | Cimpl.CBMul -> text "*"
  | Cimpl.CBDiv -> text "/"
  | Cimpl.CBMod -> text "%"
  
let format_uop (x : Cimpl.uop) = 
  match x with 
  | Cimpl.CUNeg -> text "-"

let rec format_cexp e =
  match e with
  | Cimpl.CEVar x ->
     format_cvar x
  | Cimpl.CEInt n ->
     Int.to_string n |> text
  | Cimpl.CEBool b ->
     Bool.to_string b |> text
  | Cimpl.CEMember(c,f) ->
    format_cexp c ++ dot ++ text f
  | Cimpl.CECompExpr (b, c1, c2) -> 
    format_cexp c1  ++ space ++ format_bop b ++ space ++ format_cexp c2
  | Cimpl.CEUniExpr (u, c) -> 
    format_uop u ++ space++ format_cexp c
  
and format_stmt s =
  match s with
  | Cimpl.CSSkip -> 
     semi
  | Cimpl.CSAssign(e1,e2) -> box(format_cexp e1 ++
                                 space ++
                                 text "=" ++
                                 space ++
                                 format_cexp e2)
  | Cimpl.CSIf (e1, b1, b2) -> 
    box~indent:4 (text "if" ++
                  text  "(" ++
                  format_cexp e1 ++
                  text ")" ++
                  text "{" ++
                  format_cblk b1 ++
                  text "}" ++
                  text "else" ++
                  text "{" ++
                  format_cblk b2 ++
                  text "}")
  | Cimpl.CSCall(f,args) ->
    box(format_cvar f ++ text "()") (* WRONG *)

and format_cblk ss =
  if List.is_empty ss then
    text "{ }"
  else
      text "{" ++
      newline ++
      text "    " ++
       box(Pretty.(format_list_sep_nl format_stmt ";" ss) ++
           text ";") ++
       newline ++
       text "}"

let format_param (ct,x) =
  format_ctyp ct ++
  space ++
  text "&" ++
  text x

let format_cdecl d =
  match d with
  | Cimpl.CDFunction(typ,name,params,body) ->
     box (format_ctyp typ ++
          space ++
          text name ++
          space ++ 
          text "(" ++
          Pretty.format_list_sep format_param "," params ++
          text ")" ++
          space ++
          format_cblk body)
  | Cimpl.CDStruct(name,fields) ->
    let format_field (t,x) =
      box (format_ctyp t ++
           space ++
           text x ++
           semi) in      
    box (text "typedef" ++
         space ++
         text "struct" ++
         space ++
         text name ++
         space ++
         text "{" ++
         newline ++
         Pretty.format_list_nl format_field fields ++
         newline ++
         text "}" ++
         space ++
         text name ++
         semi)
        
let format_program p =
  match p with
  | Cimpl.CProgram(ds) ->
     box (Pretty.(format_list_nl) format_cdecl ds) ++ (text "\n")
                                                 
