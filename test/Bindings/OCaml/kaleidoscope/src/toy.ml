
(** Main driver code. *)

open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

let main () =
  ignore (initialize_native_target ());

  (* Create the JIT. *)
  let execution_engine = ExecutionEngine.create Codegen.the_module in
  let fpm = Llvm.PassManager.create_function Codegen.the_module in

  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  DataLayout.add_to_pass_manager
    fpm
    (ExecutionEngine.data_layout execution_engine) ;

  (* Promote allocas to registers. *)
  add_memory_to_register_promotion fpm;

  (* Do simple "peephole" optimizations and bit-twiddling option. *)
  add_instruction_combination fpm;

  (* reassociate expressions. *)
  add_reassociation fpm;

  (* Eliminate Common SubExpressions. *)
  add_gvn fpm;

  add_constant_propagation fpm ;

  add_sccp fpm ;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_cfg_simplification fpm;

  ignore @@ Llvm.PassManager.initialize fpm;

  (* Run the main "interpreter loop" now. *)
  Toplevel.main_loop fpm execution_engine ;

  (* Print out all the generated code. *)
  Llvm.dump_module Codegen.the_module


let () = main ()
