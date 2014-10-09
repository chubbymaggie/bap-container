(** Basic type declarations for DWARF format.  *)

open Core_kernel.Std


(* TODO use bap-types endian type. *)
type endian = LittleEndian | BigEndian
with sexp,bin_io,compare

(* NB: later we will move to bitvector, but now it is impossible *)
module Addr = struct
  type t =
    | Int64 of int64
    | Int32 of int32
  with sexp,bin_io,compare,variants
end

type addr = Addr.t with sexp,bin_io,compare

(** File sections  *)
module Section = struct
  type t =
    | Info
    | Abbrev
    | Str
  with sexp,bin_io,compare,variants
end


(** Debug Entry Tag  *)
module Tag = struct
  type t =
    | Compile_unit
    | Partial_unit
    | Subprogram
    | Entry_point
    | Inlined_subroutine
    | Unknown of int
  with sexp,bin_io,compare,variants
end


(** Attribute  *)
module Attr = struct
  type t =
    | Name
    | Low_pc
    | High_pc
    | Entry_pc
    | Unknown of int
  with sexp,bin_io,compare,variants
end

type lenspec =
  | Leb128
  | One
  | Two
  | Four
  | Eight
with sexp,bin_io,compare

(** Attribute form  *)
module Form = struct
  type t =
    | Addr
    | String
    | Block of lenspec
    | Const of lenspec
    | Flag_present
    | Strp
    | Ref of lenspec
    | Indirect
    | Offset
    | Expr
    | Sig
  with sexp,bin_io,compare,variants
end

type tag  = Tag.t  with sexp,bin_io,compare
type attr = Attr.t with sexp,bin_io,compare
type form = Form.t with sexp,bin_io,compare
type section = Section.t with sexp,bin_io,compare
