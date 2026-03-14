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
        Format.printf "%s@\n%!"
          (prog |> Types.program_to_yojson |> Yojson.Safe.pretty_to_string)
      end;
    `Ok prog
  with
  | err ->
      if verbose then Format.eprintf "[%s] %s@\n%!" (Conf.red "Failed") p4_file;
      `Error (Lexer.info lexbuf, err)

let strip_known_extension s =
  if String.is_suffix s ~suffix:".p4"
  then Filename.chop_extension s
  else if String.is_suffix s ~suffix:".stf"
  then Filename.chop_extension s
  else s

let normalize_exclusion_entry s =
  let base = Filename.basename s in
  let stem = strip_known_extension base in
  match String.rsplit2 stem ~on:'_' with
  | Some (prefix, suffix) when String.for_all suffix ~f:Char.is_digit ->
      String.Set.of_list
        [ stem ^ ".stf"
        ; prefix ^ "__" ^ suffix ^ ".stf"
        ]
  | _ ->
      String.Set.singleton (stem ^ ".stf")

let read_exclusions excl_file =
  In_channel.read_lines excl_file
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:normalize_exclusion_entry
  |> List.fold ~init:String.Set.empty ~f:Set.union

let read_all_exclusions excl_files =
  List.fold excl_files ~init:String.Set.empty ~f:(fun acc file ->
      Set.union acc (read_exclusions file))

let main include_dir exclusions stf_tests_dir =
  get_stf_files stf_tests_dir
  |> List.map ~f:(fun x ->
         let stf_file = Filename.concat stf_tests_dir x in
         let p4_file = Stdlib.Filename.remove_extension stf_file ^ ".p4" in
         if Set.mem exclusions x then
           Alcotest.test_case (Filename.basename p4_file) `Quick
             (fun () -> Alcotest.skip ())
         else
           match parse_file include_dir p4_file false with
           | `Ok p4_prog -> stf_alco_test stf_file p4_file p4_prog
           | `Error _ ->
               let fail_alcotest () =
                 Alcotest.failf "petr4 couldn't parse the p4 prog: %s" p4_file
               in
               Alcotest.test_case (Filename.basename p4_file) `Quick fail_alcotest)

let () =
  let excl_files = ref [] in
  let testdirs = ref [] in

  let anon_fun s =
    testdirs := !testdirs @ [s]
  in

  let speclist =
    [
      ( "-e",
        Arg.String (fun s -> excl_files := s :: !excl_files),
        "Path to a file containing STF/P4 filenames to exclude, one per line; may be passed multiple times" );
    ]
  in

  let usage = "test.exe [-e exclude_file ...] [testdir ...]" in
  Arg.parse speclist anon_fun usage;

  let testdirs =
    if List.is_empty !testdirs then
      begin
        print_endline "No argument supplied. Running tests in ./testdata";
        [ "./testdata/v1model-tests"; "./testdata/ebpf-tests" ]
      end
    else
      !testdirs
  in

  let exclusions = read_all_exclusions !excl_files in

  let test_suite =
    List.map testdirs ~f:(fun testdir ->
        (testdir, main ["examples/"] exclusions testdir))
  in

  Alcotest.run ~argv:[| "test" |] "Stf-tests" test_suite
