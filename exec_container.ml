open Core_kernel.Std

module B = Bitvector
type addr = Bitvector.t
type asm = string option
type bil = Bil.stmt list
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
         | [sect] ->
           let e' = if B.compare sect.end_addr e < 0
                       then sect.end_addr
                       else e in
           let data = String.sub sect.data
                                 (Z.to_int (B.to_zarith (B.minus s sect.start_addr)))
                                 (Z.to_int (B.to_zarith (B.minus e' s))) in
           data ^ (get_bytes mm (B.incr e') e)
         | (_ :: _ :: _) -> failwith "Two sections define the same byte"
end
type exec_container = {memory : MemMap.t;
                       symbols : symbol list;
                       arch : string}
let get_bytes ec s e = MemMap.get_bytes ec.memory s e
let get_byte ec s = (get_bytes ec s s).[0]
let get_sections ec = ec.memory
let get_func_symbols ec = List.map ~f:(fun sym -> sym.symVal) (List.filter ~f:(fun sym -> sym.symType = Function) ec.symbols)
let get_disasm ec addr =
  let module LocalArch = (val
    (match ec.arch with
      | "arm" -> (module Arch_arm.ARM : Arch.ARCH)
      | "x86" -> (module Arch_i386.X86_32 : Arch.ARCH)
      | "x86_64" -> (module Arch_i386.X86_64 : Arch.ARCH)
      | arch -> failwith ("Unsupported architecture: " ^ arch)) : Arch.ARCH) in
   let (_, bil, ft, asm) = LocalArch.disasm LocalArch.init_state
     (get_byte ec)
     addr
   in (asm, bil, ft)
let empty = {memory = []; symbols = []; arch = ""}
let set_arch ec arch = {ec with arch = arch}
let add_section ec sect = {ec with memory = MemMap.add_section ec.memory sect}
