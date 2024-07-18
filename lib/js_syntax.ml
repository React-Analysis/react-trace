let cat (filename : string) : string =
  let ic = open_in_bin filename in
  let len = try in_channel_length ic with Sys_error _ -> 0 in
  (* in_channel_length returns 0 for non-regular files; try reading it using a
     fixed-sized buffer if it appears to be empty. NOTE: JaneStreet's Core Sys
     module defines a function is_file which does a proper check on whether the
     file exists and is regular. *)
  if len > 0 then (
    let buf = Buffer.create len in
    Buffer.add_channel buf ic len;
    close_in ic;
    Buffer.contents buf)
  else
    let len = 1024 in
    (* for Buffer, that's the initial size of the internal byte sequence *)
    let buf = Buffer.create len in
    let bytes = Bytes.create len in
    let rec read_bytes () : unit =
      try
        let n = input ic bytes 0 len in
        if n = 0 then ()
        else (
          Buffer.add_subbytes buf bytes 0 n;

          (* 0 is offset *)
          read_bytes ())
      with End_of_file -> ()
    in
    read_bytes ();
    close_in ic;
    Buffer.contents buf

let test () =
  let filename = "samples/comp.tsx" in
  let program, es =
    Parser_flow.program_file ~fail:false
      ~parse_options:
        (Some { Parser_env.default_parse_options with components = true })
      (cat filename) (Some (File_key.SourceFile filename))
  in
  let show = Flow_ast.Program.show Loc.pp Loc.pp in
  show program |> print_endline;
  List.iter (fun (_, e) -> Parse_error.PP.error e |> print_endline) es;
  ()
