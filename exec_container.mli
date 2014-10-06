open Core_kernel.Std

type addr = Bitvector.t
type perm = R | W | X
type sym_type = Unknown | Function
type symbol = {symVal  : Bitvector.t;
               symType : sym_type}
type section = {start_addr   : addr;
                end_addr     : addr;
                data        : string;
                permissions : perm list}
type t

module Reader : sig
val get_bytes : t -> addr -> addr -> string
val get_byte  : t -> addr -> char
val get_sections : t -> section list
val get_func_symbols : t -> addr list
val get_arch : t -> Arch.arch option
end

module Loader : sig
val add_section : t -> section -> t
val empty : t
val set_arch : t -> Arch.arch option -> t
end
