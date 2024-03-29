open Core_kernel.Std
open Dwarf

type t
type fn with sexp,bin_io,compare


(** [create data] tries to create a DWARF reader, from
    supplied [data]. May yield an error, if there wasn't sufficient
    sections, or if format is not understandable.

    To provide information about functions parser needs at least this
    three sections:

    - .debug_abbrev [Section.Abbr]
    - .debug_info   [Section.Info]
    - .debug_str    [Section.Str]
 *)
val create: string Dwarf_data.t -> t Or_error.t

(** [functions searcher] enumerates functions  *)
val functions: t -> (string * fn) Sequence.t

(** Current function representation.  *)
module Fn : sig
  type t = fn
  val pc_lo: t -> addr
  val pc_hi: t -> addr option
  include Identifiable.S with type t := t
end
