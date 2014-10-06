open Core_kernel.Std

open Elf
open Exec_container
open Loader
open Arch

let lit64 = Bitvector.lit64

let rec conv_flags = function
  | [] -> []
  | (PF_X :: flags) -> X :: conv_flags flags
  | (PF_W :: flags) -> W :: conv_flags flags
  | (PF_R :: flags) -> R :: conv_flags flags
  | (_ :: flags) -> conv_flags flags

let proc_segment width seg ec =
  add_section ec
    {start_addr  = lit64 seg.p_vaddr width;
     end_addr    = lit64 (Int64.(seg.p_vaddr + seg.p_memsz)) width;
     data        = seg.p_data;
     permissions = conv_flags seg.p_flags}

let load_executable path =
  match parse path with
    | Some elf -> Some (
       let (width, arch) =
         match elf.e_machine with
           | EM_386 -> (32, Some X86_32)
           | EM_ARM -> (32, Some ARM)
           | EM_X86_64 -> (64, Some X86_64)
           | _ -> (64, None) in
       set_arch
         (List.fold_right ~f:(proc_segment width) ~init:empty (elf.e_segments))
         arch)
    | None -> None
