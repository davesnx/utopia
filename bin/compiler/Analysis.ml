type origin = { line : int; column : int }
type token = { text : string; origin : origin; byte_offset : int }
type block_flavor = Ocaml_block | Slash_block
type mode = Code | String | Line_comment | Block_comment of block_flavor * int

type summary = {
  before_export_origin : origin option;
  paths_origin : origin option;
  react_client_component_origins : origin list;
}

let starts_with_at text index pattern =
  let pattern_len = String.length pattern in
  index + pattern_len <= String.length text
  && String.sub text index pattern_len = pattern

let is_identifier_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let parse_char_literal_end text start =
  if not (starts_with_at text start "'") then None
  else
    let len = String.length text in
    let rec loop index escaped =
      if index >= len then None
      else
        match text.[index] with
        | '\n' -> None
        | _ when escaped -> loop (index + 1) false
        | '\\' -> loop (index + 1) true
        | '\'' -> Some (index + 1)
        | _ -> loop (index + 1) false
    in
    loop (start + 1) false

let scan_code_tokens source =
  let len = String.length source in
  let index = ref 0 in
  let line = ref 1 in
  let column = ref 1 in
  let mode = ref Code in
  let escape_next = ref false in
  let tokens = ref [] in
  let add_token text token_line token_column token_byte_offset =
    tokens :=
      {
        text;
        origin = { line = token_line; column = token_column };
        byte_offset = token_byte_offset;
      }
      :: !tokens
  in
  let advance_char ch =
    if ch = '\n' then (
      incr line;
      column := 1)
    else incr column;
    incr index
  in
  let advance_one () = if !index < len then advance_char source.[!index] in
  let advance_two () =
    advance_one ();
    advance_one ()
  in
  while !index < len do
    match !mode with
    | Code -> (
        if starts_with_at source !index "(*" then (
          mode := Block_comment (Ocaml_block, 1);
          advance_two ())
        else if starts_with_at source !index "/*" then (
          mode := Block_comment (Slash_block, 1);
          advance_two ())
        else if starts_with_at source !index "//" then (
          mode := Line_comment;
          advance_two ())
        else if source.[!index] = '"' then (
          mode := String;
          escape_next := false;
          advance_one ())
        else
          match parse_char_literal_end source !index with
          | Some end_index ->
              while !index < end_index do
                advance_one ()
              done
          | None ->
              if is_identifier_start source.[!index] then (
                let token_line = !line in
                let token_column = !column in
                let token_byte = !index in
                advance_one ();
                while !index < len && is_identifier_char source.[!index] do
                  advance_one ()
                done;
                add_token
                  (String.sub source token_byte (!index - token_byte))
                  token_line token_column token_byte)
              else if
                source.[!index] = ' '
                || source.[!index] = '\t'
                || source.[!index] = '\r'
                || source.[!index] = '\n'
              then advance_one ()
              else
                let token_line = !line in
                let token_column = !column in
                let token_byte = !index in
                let token_text = String.make 1 source.[!index] in
                add_token token_text token_line token_column token_byte;
                advance_one ())
    | String ->
        if !escape_next then (
          escape_next := false;
          advance_one ())
        else if source.[!index] = '\\' then (
          escape_next := true;
          advance_one ())
        else if source.[!index] = '"' then (
          mode := Code;
          advance_one ())
        else advance_one ()
    | Line_comment ->
        if source.[!index] = '\n' then (
          mode := Code;
          advance_one ())
        else advance_one ()
    | Block_comment (flavor, depth) ->
        let open_token, close_token =
          match flavor with
          | Ocaml_block -> ("(*", "*)")
          | Slash_block -> ("/*", "*/")
        in
        if starts_with_at source !index open_token then (
          mode := Block_comment (flavor, depth + 1);
          advance_two ())
        else if starts_with_at source !index close_token then (
          if depth = 1 then mode := Code
          else mode := Block_comment (flavor, depth - 1);
          advance_two ())
        else advance_one ()
  done;
  List.rev !tokens

let find_sequence_origin ?(origin_index = 0) tokens pattern =
  let tokens_array = Array.of_list tokens in
  let token_count = Array.length tokens_array in
  let pattern_len = List.length pattern in
  let pattern = Array.of_list pattern in
  let max_start = token_count - pattern_len in
  let rec loop start =
    if start > max_start then None
    else
      let rec matches offset =
        if offset = pattern_len then true
        else if tokens_array.(start + offset).text = pattern.(offset) then
          matches (offset + 1)
        else false
      in
      if matches 0 then Some tokens_array.(start + origin_index).origin
      else loop (start + 1)
  in
  if pattern_len = 0 || origin_index < 0 || origin_index >= pattern_len then
    None
  else loop 0

let find_sequence_origins ?(origin_index = 0) tokens pattern =
  let tokens_array = Array.of_list tokens in
  let token_count = Array.length tokens_array in
  let pattern_len = List.length pattern in
  let pattern = Array.of_list pattern in
  let max_start = token_count - pattern_len in
  let rec loop start acc =
    if start > max_start then List.rev acc
    else
      let rec matches offset =
        if offset = pattern_len then true
        else if tokens_array.(start + offset).text = pattern.(offset) then
          matches (offset + 1)
        else false
      in
      if matches 0 then
        loop (start + 1) (tokens_array.(start + origin_index).origin :: acc)
      else loop (start + 1) acc
  in
  if pattern_len = 0 || origin_index < 0 || origin_index >= pattern_len then []
  else loop 0 []

let analyze source =
  let tokens = scan_code_tokens source in
  {
    before_export_origin =
      find_sequence_origin ~origin_index:1 tokens [ "let"; "before" ];
    paths_origin =
      find_sequence_origin ~origin_index:1 tokens [ "let"; "paths" ];
    react_client_component_origins =
      find_sequence_origins ~origin_index:2 tokens
        [ "["; "@"; "react"; "."; "client"; "."; "component"; "]" ];
  }

let string_of_origin origin = Printf.sprintf "%d:%d" origin.line origin.column
