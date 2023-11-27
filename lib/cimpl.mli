(** Cimpl is a small subset of C. It is not intended to capture the
 *  entire language, but rather to serve as a compilation target for
 *  Petr4. 
 * 
 * It inherits the usual conventions of C. For instance there
 *  are not booleans and instead `0` is interpreted as `false` and any
 *  non-zero value is interpreted as `true`.  
 * 
 * The datatypes in this file use the following conventions:
 * - All constructors start with `C`
 * - The next letter indicates the "kind" of term that it is:
 *   + `T` for types
 *   + `E` for expressions
 *   + `D` for declarations
 *   and so on...
 *)

type ctyp =
  | CTVoid
  | CTChar
  | CTInt
  | CTUInt
  | CTLInt
  | CTULInt
  | CTArray of ctyp
  | CTStruct of string

type cvar = string

type bop = 
  | CBEq
  | CBNe
  | CBLt
  | CBGt
  | CBGte
  | CBLte
  | CBAnd  
  | CBOr
  | CBAdd 
  | CBSub 
  | CBMul 
  | CBDiv 
  | CBMod 
  
type uop = 
  | CUNeg
  
type cexpr =
  | CEVar of cvar
  | CEBool of bool
  | CEInt of int
  | CEMember of cexpr * string
  | CECompExpr of bop * cexpr * cexpr
  | CEUniExpr of uop * cexpr                 

type cstmt =
  | CSSkip
  | CSAssign of cexpr * cexpr
  | CSIf of cexpr * cblk * cblk
  | CSCall of cvar * cexpr list
               
and cblk = cstmt list

type cparam = ctyp * string

type cdecl =
  | CDStruct of string * (ctyp * string) list
  | CDFunction of ctyp * string * cparam list * cblk

type cprog =
  | CProgram of cdecl list 
