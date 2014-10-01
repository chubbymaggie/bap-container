type addr = Bitvector.t
type asm = string option
type bil = Bil.stmt list
type perm = R | W | X
type sym_type = Unknown | Function
type symbol = {symVal  : Bitvector.t;
               symType : sym_type}
type section = {startAddr   : addr;
                endAddr     : addr;
                data        : string;
                permissions : perm list}
type exec_container
val get_bytes : exec_container -> addr -> addr -> string
val get_sections : exec_container -> section list
val get_func_symbols : exec_container -> addr list
val get_disasm : exec_container -> addr -> asm * bil * addr