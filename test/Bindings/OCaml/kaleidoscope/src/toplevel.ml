(** Top-Level loop and JIT Driver *)

open Llvm_executionengine

let execute_statement fpm execution_engine=
  let open Ast in function
    | Def func ->
        print_endline "Execute a function definition.";
        Llvm.dump_value (Codegen.codegen_func fpm func);
    | Extern proto ->
        print_endline "Execute an extern.";
        Llvm.dump_value (Codegen.codegen_proto proto);
    | Expr e -> begin
        (* Evaluate a top-level expression into an anonymous function. *)
        print_endline "Execute a top-level expr";
        let func = anon_function e in
        let anonym_fun = Codegen.codegen_func fpm func in
        Llvm.dump_value anonym_fun;

        (* JIT the function, returning a function pointer. *)
        let result =
          ExecutionEngine.run_function anonym_fun [||]
            execution_engine
        in

        Printf.printf
          "Evaluated to %f\n"
          @@ GenericValue.as_float Codegen.double_type result
      end


let rec main_loop fpm execution_engine : unit =
  Printf.printf "ready> %!";
  match
    let lexbuf = Syntax.create_lexbuf @@
      Sedlexing.Utf8.from_channel stdin in
    Syntax.parse_statement lexbuf
  with
    (* A statement, Execute it! *)
    | Some statement -> begin
        print_endline @@ Ast.show_statement statement ;
        execute_statement fpm execution_engine statement ;
        main_loop fpm execution_engine
      end

    (* An error, Print it! *)
    | exception Syntax.ParseError e -> begin
        print_endline @@ Syntax.string_of_ParseError e ;
        main_loop fpm execution_engine
      end
    | exception Codegen.Error s -> begin
        print_endline s ;
        main_loop fpm execution_engine
      end

    (* EOF, Exit. *)
    | None -> print_newline ()
