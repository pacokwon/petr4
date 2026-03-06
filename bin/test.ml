open Petr4
open Core
open P4stf.Test

let print pp = Format.printf "%a@." Pp.to_fmt pp

let parse_file include_dirs p4_file verbose =
  let () = Lexer.reset () in
  let () = Lexer.set_filename p4_file in
  let p4_string = Conf.preprocess include_dirs p4_file in
  let lexbuf = Lexing.from_string p4_string in
  try
    let prog = Parser.p4program Lexer.lexer lexbuf in
    if verbose then
      begin
        Format.eprintf "[%s] %s@\n%!" (Conf.green "Passed") p4_file;
        prog |> Pretty.format_program |> print;
        Format.print_string "@\n%!"; 
        Format.printf "----------@\n";
        Format.printf "%s@\n%!" (prog |> Types.program_to_yojson |> Yojson.Safe.pretty_to_string)
      end;
    `Ok prog
  with
  | err ->
    if verbose then Format.eprintf "[%s] %s@\n%!" (Conf.red "Failed") p4_file;
    `Error (Lexer.info lexbuf, err)


let main include_dir stf_tests_dir =
  get_stf_files stf_tests_dir
  |> List.map ~f:(fun x ->
    let stf_file = Filename.concat stf_tests_dir x in
    let p4_file = Stdlib.Filename.remove_extension stf_file ^ ".p4" in
    match parse_file include_dir p4_file false with
    | `Ok p4_prog -> stf_alco_test stf_file p4_file p4_prog
    | `Error e ->
        let fail_alcotest () =
          Alcotest.failf "petr4 couldn't parse the p4 prog: %s" p4_file
        in
        Alcotest.test_case (Filename.basename p4_file) `Quick fail_alcotest)

let excl stf_tests_dir =
  get_stf_files stf_tests_dir
  |> List.map ~f:(fun x ->
    let stf_file = Filename.concat stf_tests_dir x in
    let p4_file = Stdlib.Filename.remove_extension stf_file ^ ".p4" in
    (Alcotest.test_case p4_file `Quick
      (fun () -> Alcotest.(check bool) p4_file true true)))

let () =
  let argv = Sys.get_argv () in
  let testdirs =
    if Array.length argv > 1 then
      Array.sub argv ~pos:1 ~len:(Array.length argv - 1)
      |> Array.to_list
    else begin
      print_endline "No argument supplied. Running tests in ./examples/checker_tests/good/";
      ["./examples/checker_tests/good/"]
    end
  in
  let test_suite =
    List.map ~f:(fun testdir -> ("", main ["examples/"] testdir)) testdirs
  in
  test_suite
  |> Alcotest.run ~argv:[| "test" |] "Stf-tests"
