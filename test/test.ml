open Core
open Petr4
(* open Common *)

let print pp = Format.printf "%a@." Pp.to_fmt pp

let red s = s
let green s = s

let preprocess include_dirs p4file =
  let cmd =
    String.concat ~sep:" "
      (["cc"]
       @ (List.map include_dirs ~f:(Printf.sprintf "-I%s")
         @ ["-undef"; "-nostdinc"; "-E"; "-x"; "c"; p4file]))
  in
  let in_chan = Core_unix.open_process_in cmd in
  let str = In_channel.input_all in_chan in
  let _ = Core_unix.close_process_in in_chan in
  str

let parse_file include_dirs p4_file verbose =
  let () = Lexer.reset () in
  let () = Lexer.set_filename p4_file in
  let p4_string = preprocess include_dirs p4_file in
  let lexbuf = Lexing.from_string p4_string in
  try
    let prog = Parser.p4program Lexer.lexer lexbuf in
    if verbose then
      begin
        Format.eprintf "[%s] %s@\n%!" (green "Passed") p4_file;
        prog |> Pretty.format_program |> print;
        Format.print_string "@\n%!";
        Format.printf "----------@\n";
        Format.printf "%s@\n%!"
          (prog |> Types.program_to_yojson |> Yojson.Safe.pretty_to_string)
      end;
    `Ok prog
  with
  | err ->
      if verbose then Format.eprintf "[%s] %s@\n%!" (red "Failed") p4_file;
      `Error (Lexer.info lexbuf, err)

let parse_string p4_string =
  let () = Lexer.reset () in
  let () = Lexer.set_filename p4_string in
  let lexbuf = Lexing.from_string p4_string in
  Parser.p4program Lexer.lexer lexbuf

let parser_test include_dirs file =
  match parse_file include_dirs file false with
  | `Ok _ -> true
  | `Error _ -> false

let to_string pp : string =
  Format.fprintf Format.str_formatter "%a" Pp.to_fmt pp;
  Format.flush_str_formatter ()

let get_name include_dirs file =
  match parse_file include_dirs file false with
  | `Ok prog -> prog |> Pretty.format_program |> to_string
  | `Error _ -> "121"

let pp_round_trip_test include_dirs file =
  let way_there =
    match parse_file include_dirs file false with
    | `Ok prog -> prog |> Pretty.format_program |> to_string
    | `Error _ -> ""
  in
  let way_back = parse_string way_there in
  String.compare way_there (way_back |> Pretty.format_program |> to_string) = 0

let typecheck_test (include_dirs : string list) (p4_file : string) : bool =
  Printf.printf "Testing file %s...\n" p4_file;
  match parse_file include_dirs p4_file false with
  | `Ok prog ->
      begin
        try
          let prog, renamer = Elaborate.elab prog in
          let _ = Checker.check_program renamer prog in
          true
        with
        | Error.Type (info, err) ->
            Format.eprintf "%s: %a" (Info.to_string info) Error.format_error err;
            false
        | exn ->
            Format.eprintf "Unknown exception: %s" (Exn.to_string exn);
            false
      end
  | `Error (_, Lexer.Error _) -> false
  | `Error (_, Parser.Error) -> false
  | `Error (_, _) -> false

let get_files path =
  Sys_unix.ls_dir path
  |> List.filter ~f:(fun name -> Core.Filename.check_suffix name ".p4")

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
        [ stem ^ ".p4"
        ; prefix ^ "__" ^ suffix ^ ".p4"
        ]
  | _ ->
      String.Set.singleton (stem ^ ".p4")

let read_exclusions excl_file =
  In_channel.read_lines excl_file
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:normalize_exclusion_entry
  |> List.fold ~init:String.Set.empty ~f:Set.union

let read_all_exclusions excl_files =
  List.fold excl_files ~init:String.Set.empty ~f:(fun acc file ->
      Set.union acc (read_exclusions file))

let good_files = "./testdata/p4_16_samples" |> get_files
let bad_files = "./testdata/p4_16_errors" |> get_files

(* This is a hack, sorry! *)
let known_failures =
  [ "default-control-argument.p4"
  ; "cast-call.p4"
  ; "issue1803_same_table_name.p4"
  ; "issue1541.p4"
  ; "issue1672-bmv2.p4"
  ; "issue1932.p4"
  ; "table-entries-optional-2-bmv2.p4"
  ; "control-verify.p4"
  ; "div1.p4"
  ; "table-entries-lpm-2.p4"
  ; "default-control-argument.p4"
  ]

let good_test f file () =
  Alcotest.(check bool) "good test" true
    (f ["./examples"] (Filename.concat "./testdata/p4_16_samples" file))

let bad_test f file () =
  Alcotest.(check bool) "bad test" false
    (f ["./examples"] (Filename.concat "./testdata/p4_16_errors" file))

let excluded_test file () =
  Format.printf "Skipping excluded test: %s@." file;
  Alcotest.skip ()

let build_cases ~excluded files mk_test =
  Stdlib.List.map
    (fun name ->
      if Set.mem excluded name then
        Alcotest.test_case name `Quick (excluded_test name)
      else
        Alcotest.test_case name `Quick (mk_test name))
    files

let () =
  let open Alcotest in
  let excl_files = ref [] in
  let run_pos = ref false in
  let run_neg = ref false in

  let speclist =
    [
      ("-e",
       Arg.String (fun s -> excl_files := s :: !excl_files),
       "File containing filenames to exclude (one per line); may be passed multiple times");
      ("-pos",
       Arg.Set run_pos,
       "Run positive tests");
      ("-neg",
       Arg.Set run_neg,
       "Run negative tests");
    ]
  in

  let usage = "test.exe [-e exclude_file ...] [-pos|-neg]" in
  Arg.parse speclist (fun _ -> ()) usage;

  if (!run_pos && !run_neg) || (not !run_pos && not !run_neg) then
    failwith "Specify exactly one of -pos or -neg";

  let excluded = read_all_exclusions !excl_files in

  let tests =
    if !run_pos then
      [
        ( "Positive Typecheck Tests",
          build_cases ~excluded good_files (good_test typecheck_test) );
      ]
    else
      [
        ( "Negative Typecheck Tests",
          build_cases ~excluded bad_files (bad_test typecheck_test) );
      ]
  in

  run ~argv:[| "test" |] "Tests" tests
