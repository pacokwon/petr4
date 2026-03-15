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

type exclude_mode =
  | P4c
  | P4testgen

let exclude_mode_of_testdir testdir =
  let testdir = String.substr_replace_all testdir ~pattern:"\\" ~with_:"/" in
  if String.is_substring testdir ~substring:"/p4testgen/"
  then P4testgen
  else if String.is_substring testdir ~substring:"/p4c/"
  then P4c
  else
    failwith
      (Printf.sprintf "could not determine exclude mode from TESTDIR: %s" testdir)

let strip_known_extension s =
  if String.is_suffix s ~suffix:".p4"
     || String.is_suffix s ~suffix:".stf"
     || String.is_suffix s ~suffix:".json"
  then Filename.chop_extension s
  else s

let is_digits s =
  String.for_all s ~f:Char.is_digit

let matching_p4testgen_family_stfs ~available_stfs stem =
  Set.filter available_stfs ~f:(fun name ->
      match String.chop_suffix name ~suffix:".stf" with
      | None -> false
      | Some base ->
          String.is_prefix base ~prefix:(stem ^ "__")
          &&
          let suffix = String.drop_prefix base (String.length stem + 2) in
          not (String.is_empty suffix) && is_digits suffix)

let normalize_exclusion_entry ~mode ~available_stfs raw =
  let raw = String.strip raw in
  if String.is_empty raw || String.is_substring raw ~substring:"p4_16_errors" then
    String.Set.empty
  else
    let name = Filename.basename raw in
    match mode with
    | P4c ->
        (* The bash script excludes exact .p4/.stf files.
           This OCaml runner executes STF tests only, so we map both to the
           corresponding STF test name. *)
        begin
          match () with
          | _ when String.is_suffix name ~suffix:".stf" ->
              if Set.mem available_stfs name
              then String.Set.singleton name
              else String.Set.empty
          | _ when String.is_suffix name ~suffix:".p4" ->
              let target = strip_known_extension name ^ ".stf" in
              if Set.mem available_stfs target
              then String.Set.singleton target
              else String.Set.empty
          | _ -> String.Set.empty
        end
    | P4testgen ->
        begin
          match () with
          | _ when String.is_suffix name ~suffix:".p4" ->
              let stem = strip_known_extension name in
              matching_p4testgen_family_stfs ~available_stfs stem
          | _ when String.is_suffix name ~suffix:".stf" ->
              let stem = strip_known_extension name in
              begin
                match String.rsplit2 stem ~on:'_' with
                | Some (prefix, suffix) when is_digits suffix ->
                    let target = prefix ^ "__" ^ suffix ^ ".stf" in
                    if Set.mem available_stfs target
                    then String.Set.singleton target
                    else String.Set.empty
                | _ -> String.Set.empty
              end
          | _ -> String.Set.empty
        end

let read_exclusions ~mode ~available_stfs excl_file =
  In_channel.read_lines excl_file
  |> List.map ~f:(normalize_exclusion_entry ~mode ~available_stfs)
  |> List.fold ~init:String.Set.empty ~f:Set.union

let read_all_exclusions ~testdir excl_files =
  let mode = exclude_mode_of_testdir testdir in
  let available_stfs = get_stf_files testdir |> String.Set.of_list in
  List.fold excl_files ~init:String.Set.empty ~f:(fun acc file ->
      Set.union acc (read_exclusions ~mode ~available_stfs file))

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

  let test_suite =
    List.map testdirs ~f:(fun testdir ->
        let exclusions = read_all_exclusions ~testdir !excl_files in
        (testdir, main ["examples/"] exclusions testdir))
  in

  Alcotest.run ~argv:[| "test" |] "Stf-tests" test_suite
