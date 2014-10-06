open Core_kernel.Std

module B = Bitvector
type addr = Bitvector.t
type perm = R | W | X
module AddrHash = struct
type 'a t = unit
end
type sym_type = Unknown | Function
type symbol = {symVal  : Bitvector.t;
               symType : sym_type}
type section = {start_addr  : addr;
                end_addr    : addr;
                data        : string;
                permissions : perm list}
module MemMap = struct 
  (* For now, we'll do this the slow way, assuming there are few sections *)
  type t = section list
  let add_section m s = s :: m
  let rec get_bytes mm s e =
    if s > e then "" else
    let sects = (List.filter ~f:(fun sect -> (sect.start_addr < s) && (sect.end_addr >= s)) mm)
    in match sects with 
         | [] -> "" (* For now, return empty string for no bytes *)
         | (sect :: _) ->
           let e' = if B.compare sect.end_addr e < 0
                       then sect.end_addr
                       else e in
           let data = String.sub sect.data
                                 (Z.to_int (B.to_zarith (B.minus s sect.start_addr)))
                                 (Z.to_int (B.to_zarith (B.minus e' s))) in
           data ^ (get_bytes mm (B.incr e') e)
end
type exec_container = {memory : MemMap.t;
                       symbols : symbol list;
                       arch : Arch.arch option}
type t = exec_container
module Reader = struct
let get_bytes ec s e = MemMap.get_bytes ec.memory s e
let get_byte ec s = (get_bytes ec s s).[0]
let get_sections ec = ec.memory
let get_func_symbols ec = List.map ~f:(fun sym -> sym.symVal) (List.filter ~f:(fun sym -> sym.symType = Function) ec.symbols)
let get_arch ec = ec.arch
end

module Loader = struct
let empty = {memory = []; symbols = []; arch = None}
let set_arch ec arch = {ec with arch = arch}
let add_section ec sect = {ec with memory = MemMap.add_section ec.memory sect}
end
