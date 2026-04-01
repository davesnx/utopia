type checklist_item = { text : string; done_ : bool }

type note_summary = {
  slug : string;
  tag_slug : string;
  route : Utopia.Route.t;
  title : string;
  preview : string;
  updated_at : string;
}

type note = {
  summary : note_summary;
  body_markdown : string;
  checklist : checklist_item list;
  tags : string list;
}

type tag_summary = {
  slug : string;
  name : string;
  route : Utopia.Route.t;
  description : string option;
  count : int;
}

let seed_tag_specs =
  [
    ("launch", "Launch", Some "Product launch");
    ("travel", "Travel", Some "Trips and logistics");
    ("design", "Design", Some "Interface notes");
    ("archive", "Archive", Some "Loose ideas");
  ]

let notes_route = Utopia.Routes.Notes.route
let new_note_route = Utopia.Routes.Notes.New.route
let tag_route slug = Utopia.Routes.Notes.Param_tag.make ~tag:slug ()
let note_route tag_slug _slug = tag_route tag_slug

let option_of_non_empty value =
  let value = String.trim value in
  if value = "" then None else Some value

let description_text description = description |> Option.value ~default:""

let split_on_dash slug =
  slug |> String.split_on_char '-' |> List.filter (fun piece -> piece <> "")

let capitalize_word word =
  if word = "" then word
  else
    (String.sub word 0 1 |> String.uppercase_ascii)
    ^ String.sub word 1 (String.length word - 1)

let humanize_tag_slug slug =
  match split_on_dash slug |> List.map capitalize_word with
  | [] -> slug
  | words -> String.concat " " words

let find_seed_tag_spec slug =
  seed_tag_specs
  |> List.find_map (fun (candidate_slug, name, description) ->
      if String.equal candidate_slug slug then Some (name, description)
      else None)

let seed_tag_name slug =
  find_seed_tag_spec slug |> Option.map fst
  |> Option.value ~default:(humanize_tag_slug slug)

let seed_tag_description slug =
  Option.bind (find_seed_tag_spec slug) (fun (_name, description) ->
      description)

let make_tag_summary ~count ~slug ~name ~description () =
  { slug; name; route = tag_route slug; description; count }

let make_note ~tag_slug ~slug ~title ~preview ~updated_at ~body_markdown
    ~checklist ~tags () =
  let summary =
    {
      slug;
      tag_slug;
      route = note_route tag_slug slug;
      title;
      preview;
      updated_at;
    }
  in
  { summary; body_markdown; checklist; tags }

let seed_notes =
  [
    make_note ~tag_slug:"launch" ~slug:"launch" ~title:"Spring launch checklist"
      ~preview:"Hero copy, footer polish, and one final empty-state pass."
      ~updated_at:"Today, 8:42 AM"
      ~body_markdown:
        "The quiet version of the launch is still the right one. Keep the \
         shell calm, leave more paper around the text, and let the product \
         screenshots breathe.\n\n\
         Homepage still needs three final touches: tighten the top navigation, \
         swap the testimonial order, and soften the empty-state language so it \
         sounds like a guide instead of a warning.\n\n\
         If there is time, add a brief handoff note for motion so the hover \
         states feel less abrupt on touch devices."
      ~checklist:
        [
          { text = "Ship the refined empty-state copy"; done_ = true };
          { text = "Finalize the pricing comparison spacing"; done_ = true };
          { text = "Record the short walkthrough for the team"; done_ = false };
        ]
      ~tags:[ "launch"; "copy"; "handoff" ]
      ();
    make_note ~tag_slug:"travel" ~slug:"travel" ~title:"Oslo offsite sketch"
      ~preview:"One bag, morning light, and a long walk after the workshop."
      ~updated_at:"Yesterday, 6:15 PM"
      ~body_markdown:
        "Aim for a one-bag setup. The smaller the footprint, the calmer the \
         trip feels once the workshop starts.\n\n\
         Need a room with good morning light, a large shared table, and one \
         dinner spot that feels tucked away enough for longer conversations.\n\n\
         Bring a paper notebook for the final day so the strategy session does \
         not disappear into laptop tabs."
      ~checklist:
        [
          { text = "Confirm the workshop room booking"; done_ = true };
          { text = "Pack the prototype printouts"; done_ = false };
          { text = "Book a quiet table for Thursday night"; done_ = false };
        ]
      ~tags:[ "travel"; "offsite"; "ops" ]
      ();
    make_note ~tag_slug:"design" ~slug:"design" ~title:"Notes redesign critique"
      ~preview:
        "The interface should feel plain, light, and almost invisible around \
         the writing."
      ~updated_at:"Today, 7:08 AM"
      ~body_markdown:
        "The best part of Apple Notes is not the color. It is the way the \
         chrome fades back so the writing can do the work.\n\n\
         One sidebar is enough. Use it for the routes, keep the separators \
         faint, and let the note itself take the full width of attention. Even \
         the shell API should stay compact: `Utopia.useRouter()` ought to be \
         enough most of the time.\n\n\
         ```mlx\n\
         let route = Utopia.Routes.Notes.Param_tag.make ~tag:\"design\" ()\n\
         router.navigate ~freshness:Utopia.Revalidate route\n\
         ```\n\n\
         Selection can be quiet too: a pale fill, a thin divider, and no \
         decorative cards, rounded corners, or shadows competing with the \
         page."
      ~checklist:
        [
          { text = "Keep the sidebar visually quiet"; done_ = true };
          { text = "Use a single neutral selection fill"; done_ = true };
          { text = "Test the note column on narrow screens"; done_ = false };
        ]
      ~tags:[ "design"; "apple-notes"; "rsc" ]
      ();
    make_note ~tag_slug:"archive" ~slug:"archive"
      ~title:"Quiet ideas worth revisiting"
      ~preview:"Reading mode, softer separators, and a weekly note shelf."
      ~updated_at:"Last week"
      ~body_markdown:
        "Reading mode could remove the route list entirely and leave only the \
         title, the date, and the note.\n\n\
         A weekly shelf might work if it behaves more like a stack of recent \
         notes than a dense calendar.\n\n\
         These are not urgent, but they still feel alive enough to keep nearby."
      ~checklist:
        [
          {
            text = "Prototype reading mode after the shell is stable";
            done_ = false;
          };
          { text = "Revisit the weekly shelf after interviews"; done_ = false };
        ]
      ~tags:[ "archive"; "future"; "reading-mode" ]
      ();
  ]

let checklist_sep = Char.chr 29
let checklist_state_sep = Char.chr 28
let tag_sep = Char.chr 27
let schema_key = "notes_schema_version"
let schema_version = "apple-notes-demo-v6"

let sql_text value =
  let pieces = String.split_on_char '\'' value in
  "'" ^ String.concat "''" pieces ^ "'"

let encode_body_markdown body_markdown =
  let buffer = Buffer.create (String.length body_markdown) in
  String.iter
    (function
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | ch -> Buffer.add_char buffer ch)
    body_markdown;
  Buffer.contents buffer

let decode_body_markdown body_markdown =
  let buffer = Buffer.create (String.length body_markdown) in
  let rec loop index =
    if index >= String.length body_markdown then ()
    else if
      index + 1 < String.length body_markdown && body_markdown.[index] = '\\'
    then (
      let next = body_markdown.[index + 1] in
      (match next with
      | '\\' -> Buffer.add_char buffer '\\'
      | 'n' -> Buffer.add_char buffer '\n'
      | 'r' -> Buffer.add_char buffer '\r'
      | 't' -> Buffer.add_char buffer '\t'
      | _ ->
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer next);
      loop (index + 2))
    else (
      Buffer.add_char buffer body_markdown.[index];
      loop (index + 1))
  in
  loop 0;
  Buffer.contents buffer

let encode_checklist items =
  items
  |> List.map (fun item ->
      (if item.done_ then "1" else "0")
      ^ String.make 1 checklist_state_sep
      ^ item.text)
  |> String.concat (String.make 1 checklist_sep)

let encode_tags tags = String.concat (String.make 1 tag_sep) tags

let split_compact sep value =
  if value = "" then []
  else
    value |> String.split_on_char sep |> List.filter (fun piece -> piece <> "")

let decode_checklist value =
  split_compact checklist_sep value
  |> List.filter_map (fun item ->
      match String.split_on_char checklist_state_sep item with
      | [ state; text ] -> Some { text; done_ = state = "1" }
      | _ -> None)

let decode_tags value = split_compact tag_sep value

let insert_tag_sql ~slug ~name ~description =
  Printf.sprintf
    "INSERT OR IGNORE INTO tags (slug, name, description) VALUES (%s, %s, %s);"
    (sql_text slug) (sql_text name)
    (sql_text (description_text description))

let insert_sql note =
  Printf.sprintf
    "INSERT INTO notes (slug, tag_slug, route, title, preview, updated_at, \
     body_markdown, checklist, tags) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, \
     %s);"
    (sql_text note.summary.slug)
    (sql_text note.summary.tag_slug)
    (sql_text (Utopia.Route.href note.summary.route))
    (sql_text note.summary.title)
    (sql_text note.summary.preview)
    (sql_text note.summary.updated_at)
    (sql_text (encode_body_markdown note.body_markdown))
    (sql_text (encode_checklist note.checklist))
    (sql_text (encode_tags note.tags))

let note_count_in notes tag_slug =
  notes
  |> List.fold_left
       (fun total note ->
         if String.equal note.summary.tag_slug tag_slug then total + 1
         else total)
       0

let take count values =
  let rec loop remaining acc rest =
    if remaining <= 0 then List.rev acc
    else
      match rest with
      | [] -> List.rev acc
      | value :: tail -> loop (remaining - 1) (value :: acc) tail
  in
  loop count [] values

let unique_preserving_order values =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | value :: rest when List.mem value seen -> loop seen acc rest
    | value :: rest -> loop (value :: seen) (value :: acc) rest
  in
  loop [] [] values

let concat_map mapper values = values |> List.map mapper |> List.flatten

let tag_slugs_from_notes notes =
  notes
  |> List.map (fun note -> note.summary.tag_slug)
  |> unique_preserving_order

let tag_summaries_from notes =
  let seed_slugs =
    List.map (fun (slug, _name, _description) -> slug) seed_tag_specs
  in
  let slugs =
    unique_preserving_order (seed_slugs @ tag_slugs_from_notes notes)
  in
  slugs
  |> List.map (fun slug ->
      make_tag_summary ~count:(note_count_in notes slug) ~slug
        ~name:(seed_tag_name slug)
        ~description:(seed_tag_description slug)
        ())

let notes_for_tag_from notes tag_slug =
  notes |> List.filter (fun note -> String.equal note.summary.tag_slug tag_slug)

let note_summaries_for_tag_from notes tag_slug =
  notes_for_tag_from notes tag_slug |> List.map (fun note -> note.summary)

let seed_tag_summary tag_slug =
  tag_summaries_from seed_notes
  |> List.find_opt (fun summary -> String.equal summary.slug tag_slug)
  |> Option.value
       ~default:
         (make_tag_summary ~count:0 ~slug:tag_slug
            ~name:(seed_tag_name tag_slug)
            ~description:(seed_tag_description tag_slug)
            ())

let summary_of_fields = function
  | [ slug; tag_slug; _route; title; preview; updated_at ] ->
      {
        slug;
        tag_slug;
        route = note_route tag_slug slug;
        title;
        preview;
        updated_at;
      }
  | fields ->
      invalid_arg
        (Printf.sprintf "Invalid note summary row with %d fields"
           (List.length fields))

let note_of_fields = function
  | [
      slug;
      tag_slug;
      _route;
      title;
      preview;
      updated_at;
      body_markdown;
      checklist;
      tags;
    ] ->
      {
        summary =
          {
            slug;
            tag_slug;
            route = note_route tag_slug slug;
            title;
            preview;
            updated_at;
          };
        body_markdown = decode_body_markdown body_markdown;
        checklist = decode_checklist checklist;
        tags = decode_tags tags;
      }
  | fields ->
      invalid_arg
        (Printf.sprintf "Invalid note row with %d fields" (List.length fields))

let tag_summary_of_fields = function
  | [ slug; name; description; count ] ->
      make_tag_summary
        ~count:(int_of_string_opt count |> Option.value ~default:0)
        ~slug ~name
        ~description:(option_of_non_empty description)
        ()
  | fields ->
      invalid_arg
        (Printf.sprintf "Invalid tag summary row with %d fields"
           (List.length fields))

let starts_with_at text index prefix =
  let prefix_len = String.length prefix in
  index + prefix_len <= String.length text
  && String.sub text index prefix_len = prefix

let rec index_of_substring text prefix index =
  if index + String.length prefix > String.length text then None
  else if starts_with_at text index prefix then Some index
  else index_of_substring text prefix (index + 1)

let executable_path () =
  if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name
  else Sys.executable_name

let db_file () =
  let executable_path = executable_path () in
  let build_marker = "/_build/default/" in
  match index_of_substring executable_path build_marker 0 with
  | Some index ->
      let build_root = String.sub executable_path 0 index in
      let remainder_start = index + String.length build_marker in
      let remainder =
        String.sub executable_path remainder_start
          (String.length executable_path - remainder_start)
      in
      let relative_dir = Filename.dirname (Filename.dirname remainder) in
      Filename.concat (Filename.concat build_root relative_dir) "notes.sqlite3"
  | None ->
      let executable_dir = Filename.dirname executable_path in
      if Filename.basename executable_dir = "_utopia" then
        Filename.concat (Filename.dirname executable_dir) "notes.sqlite3"
      else Filename.concat (Sys.getcwd ()) "notes.sqlite3"

let db_initialized = ref false

let raw_sql_lines sql =
  [%platform
    match () with
    | Server ->
        let remove_if_exists path =
          if Sys.file_exists path then Sys.remove path
        in
        let read_file path =
          In_channel.with_open_bin path In_channel.input_all
        in
        let sql_path = Filename.temp_file "utopia-notes" ".sql" in
        let output_path = Filename.temp_file "utopia-notes" ".out" in
        let error_path = Filename.temp_file "utopia-notes" ".err" in
        Fun.protect
          ~finally:(fun () ->
            remove_if_exists sql_path;
            remove_if_exists output_path;
            remove_if_exists error_path)
          (fun () ->
            Out_channel.with_open_bin sql_path (fun channel ->
                output_string channel ".mode tabs\n.headers off\n";
                output_string channel sql;
                output_char channel '\n');
            let db_path = db_file () in
            let command =
              Printf.sprintf "sqlite3 -batch %s < %s > %s 2> %s"
                (Filename.quote db_path) (Filename.quote sql_path)
                (Filename.quote output_path)
                (Filename.quote error_path)
            in
            match Sys.command command with
            | 0 ->
                read_file output_path |> String.split_on_char '\n'
                |> List.filter (fun line -> String.trim line <> "")
            | code ->
                let error = read_file error_path |> String.trim in
                failwith (Printf.sprintf "sqlite3 failed (%d): %s" code error))
    | Client ->
        let _ = sql in
        []]

let ensure_schema () =
  ignore
    (raw_sql_lines
       "CREATE TABLE IF NOT EXISTS utopia_meta (key TEXT PRIMARY KEY, value \
        TEXT NOT NULL);");
  let current_version =
    match
      raw_sql_lines
        (Printf.sprintf "SELECT value FROM utopia_meta WHERE key = %s LIMIT 1;"
           (sql_text schema_key))
    with
    | value :: _ -> Some value
    | [] -> None
  in
  if current_version <> Some schema_version then (
    ignore (raw_sql_lines "DROP TABLE IF EXISTS notes;");
    ignore (raw_sql_lines "DROP TABLE IF EXISTS tags;");
    ignore
      (raw_sql_lines
         "CREATE TABLE tags (slug TEXT PRIMARY KEY, name TEXT NOT NULL, \
          description TEXT NOT NULL);");
    ignore
      (raw_sql_lines
         "CREATE TABLE notes (slug TEXT PRIMARY KEY, tag_slug TEXT NOT NULL, \
          route TEXT NOT NULL, title TEXT NOT NULL, preview TEXT NOT NULL, \
          updated_at TEXT NOT NULL, body_markdown TEXT NOT NULL, checklist \
          TEXT NOT NULL, tags TEXT NOT NULL);");
    ignore
      (raw_sql_lines
         (Printf.sprintf
            "INSERT OR REPLACE INTO utopia_meta (key, value) VALUES (%s, %s);"
            (sql_text schema_key) (sql_text schema_version))))

let ensure_db () =
  [%platform
    match () with
    | Server ->
        if not !db_initialized then (
          ensure_schema ();
          let existing_tag_count =
            match raw_sql_lines "SELECT COUNT(*) FROM tags;" with
            | count :: _ -> int_of_string_opt count |> Option.value ~default:0
            | [] -> 0
          in
          if existing_tag_count = 0 then
            seed_tag_specs
            |> List.iter (fun (slug, name, description) ->
                ignore (raw_sql_lines (insert_tag_sql ~slug ~name ~description)));
          let existing_note_count =
            match raw_sql_lines "SELECT COUNT(*) FROM notes;" with
            | count :: _ -> int_of_string_opt count |> Option.value ~default:0
            | [] -> 0
          in
          if existing_note_count = 0 then
            seed_notes
            |> List.iter (fun note -> ignore (raw_sql_lines (insert_sql note)));
          db_initialized := true)
    | Client -> ()]

let query_lines sql =
  ensure_db ();
  raw_sql_lines sql

let list_notes tag_slug : note_summary list =
  [%platform
    match () with
    | Server ->
        let sql =
          Printf.sprintf
            "SELECT slug, tag_slug, route, title, preview, updated_at FROM \
             notes WHERE tag_slug = %s ORDER BY rowid DESC;"
            (sql_text tag_slug)
        in
        query_lines sql
        |> List.map (fun line ->
            line |> String.split_on_char '\t' |> summary_of_fields)
    | Client -> note_summaries_for_tag_from seed_notes tag_slug]

let notes_for_tag tag_slug : note list =
  [%platform
    match () with
    | Server ->
        let sql =
          Printf.sprintf
            "SELECT slug, tag_slug, route, title, preview, updated_at, \
             body_markdown, checklist, tags FROM notes WHERE tag_slug = %s \
             ORDER BY rowid DESC;"
            (sql_text tag_slug)
        in
        query_lines sql
        |> List.map (fun line ->
            line |> String.split_on_char '\t' |> note_of_fields)
    | Client -> notes_for_tag_from seed_notes tag_slug]

let tag_summaries () : tag_summary list =
  [%platform
    match () with
    | Server ->
        let sql =
          "SELECT tags.slug, tags.name, tags.description, COUNT(notes.slug) \
           FROM tags LEFT JOIN notes ON notes.tag_slug = tags.slug GROUP BY \
           tags.slug, tags.name, tags.description ORDER BY tags.rowid ASC;"
        in
        query_lines sql
        |> List.map (fun line ->
            line |> String.split_on_char '\t' |> tag_summary_of_fields)
    | Client -> tag_summaries_from seed_notes]

let all_note_summaries () : note_summary list =
  tag_summaries () |> concat_map (fun summary -> list_notes summary.slug)

let all_notes () : note list =
  tag_summaries () |> concat_map (fun summary -> notes_for_tag summary.slug)

let recent_notes () = all_note_summaries () |> take 4

let tag_summary_opt tag_slug =
  tag_summaries ()
  |> List.find_opt (fun summary -> String.equal summary.slug tag_slug)

let tag_summary tag_slug =
  tag_summary_opt tag_slug |> Option.value ~default:(seed_tag_summary tag_slug)

let body_text_of_html html =
  let buffer = Buffer.create (String.length html) in
  let rec loop index inside_tag =
    if index >= String.length html then ()
    else
      let ch = html.[index] in
      if inside_tag then
        if ch = '>' then loop (index + 1) false else loop (index + 1) true
      else if ch = '<' then loop (index + 1) true
      else (
        Buffer.add_char buffer ch;
        loop (index + 1) false)
  in
  loop 0 false;
  Buffer.contents buffer

let html_escaped_string text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | ch -> Buffer.add_char buffer ch)
    text;
  Buffer.contents buffer

let collapse_whitespace text =
  let buffer = Buffer.create (String.length text) in
  let rec loop index last_space =
    if index >= String.length text then ()
    else
      let ch = text.[index] in
      let is_space =
        ch = ' ' || ch = '\n' || ch = '\r' || ch = '\t' || ch = Char.chr 160
      in
      if is_space then (
        if not last_space then Buffer.add_char buffer ' ';
        loop (index + 1) true)
      else (
        Buffer.add_char buffer ch;
        loop (index + 1) false)
  in
  loop 0 true;
  Buffer.contents buffer |> String.trim

let trim_dashes text =
  let length = String.length text in
  let rec left index =
    if index >= length then length
    else if text.[index] = '-' then left (index + 1)
    else index
  in
  let rec right index =
    if index < 0 then -1
    else if text.[index] = '-' then right (index - 1)
    else index
  in
  let start = left 0 in
  let finish = right (length - 1) in
  if finish < start then "" else String.sub text start (finish - start + 1)

let strip_leading_hashes text =
  let length = String.length text in
  let rec loop index =
    if index >= length then ""
    else
      match text.[index] with
      | '#' | ' ' | '\t' -> loop (index + 1)
      | _ -> String.sub text index (length - index)
  in
  loop 0

let normalize_tag_slug text =
  let text = text |> String.trim |> strip_leading_hashes in
  let buffer = Buffer.create (String.length text) in
  let rec loop index previous_dash =
    if index >= String.length text then ()
    else
      let ch = Char.lowercase_ascii text.[index] in
      let is_alnum = ('a' <= ch && ch <= 'z') || ('0' <= ch && ch <= '9') in
      if is_alnum then (
        Buffer.add_char buffer ch;
        loop (index + 1) false)
      else if previous_dash then loop (index + 1) true
      else (
        Buffer.add_char buffer '-';
        loop (index + 1) true)
  in
  loop 0 true;
  Buffer.contents buffer |> trim_dashes

let is_reserved_tag_slug slug = String.equal slug "new"

let time_only updated_at =
  match String.rindex_opt updated_at ',' with
  | Some index when index + 1 < String.length updated_at ->
      let value =
        String.sub updated_at (index + 1) (String.length updated_at - index - 1)
        |> String.trim
      in
      if value = "" then updated_at else value
  | _ -> updated_at

let preview_of_body_markdown body_markdown =
  let plain =
    [%platform
      match () with
      | Server ->
          body_markdown |> Utopia_markdown.render_string_to_html
          |> body_text_of_html |> collapse_whitespace
      | Client -> collapse_whitespace body_markdown]
  in
  if plain = "" then "New note"
  else if String.length plain <= 110 then plain
  else String.sub plain 0 107 ^ "..."

let normalize_body_markdown body_markdown = String.trim body_markdown

let render_note_body_html body_markdown =
  [%platform
    match () with
    | Server -> Utopia_markdown.render_string_to_html body_markdown
    | Client -> html_escaped_string body_markdown]

let timestamp_slug tag_slug title =
  let sanitized = normalize_tag_slug title in
  [%platform
    match () with
    | Server ->
        let suffix =
          Unix.gettimeofday () *. 1000. |> int_of_float |> string_of_int
        in
        let base = if sanitized = "" then tag_slug else sanitized in
        base ^ "-" ^ suffix
    | Client -> if sanitized = "" then tag_slug ^ "-note" else sanitized]

let note_by_slug slug =
  [%platform
    match () with
    | Server ->
        let sql =
          Printf.sprintf
            "SELECT slug, tag_slug, route, title, preview, updated_at, \
             body_markdown, checklist, tags FROM notes WHERE slug = %s LIMIT \
             1;"
            (sql_text slug)
        in
        query_lines sql
        |> List.find_map (fun line ->
            match String.split_on_char '\t' line with
            | [] -> None
            | fields -> Some (note_of_fields fields))
    | Client ->
        seed_notes |> List.find_opt (fun note -> note.summary.slug = slug)]

let toggle_checklist_item_at item_index items =
  let toggled = ref false in
  let items =
    items
    |> List.mapi (fun index item ->
        if index = item_index then (
          toggled := true;
          { item with done_ = not item.done_ })
        else item)
  in
  (!toggled, items)

let update_note_checklist_sql ~slug checklist =
  Printf.sprintf "UPDATE notes SET checklist = %s WHERE slug = %s;"
    (sql_text (encode_checklist checklist))
    (sql_text slug)

let toggle_note_checklist_item slug item_index =
  [%platform
    match () with
    | Server -> (
        match note_by_slug slug with
        | None -> notes_route
        | Some note ->
            let route = note.summary.route in
            if item_index < 0 then route
            else
              let did_toggle, checklist =
                toggle_checklist_item_at item_index note.checklist
              in
              if did_toggle then
                ignore (query_lines (update_note_checklist_sql ~slug checklist));
              route)
    | Client ->
        let _ = item_index in
        note_by_slug slug
        |> Option.map (fun note -> note.summary.route)
        |> Option.value ~default:notes_route]

let toggle_note_checklist_item_from_form_data formData =
  [%platform
    match () with
    | Server ->
        let get_string name =
          Js.FormData.get formData name |> function
          | `String value -> value |> String.trim
        in
        let slug = get_string "slug" in
        let item_index =
          get_string "item_index" |> int_of_string_opt
          |> Option.value ~default:(-1)
        in
        toggle_note_checklist_item slug item_index
    | Client ->
        let _ = formData in
        notes_route]

let create_or_lookup_tag ~name ~description =
  let name = String.trim name in
  let slug = normalize_tag_slug name in
  let _ = description in
  if name = "" then invalid_arg "Tag name cannot be blank";
  if slug = "" then invalid_arg "Tag name cannot be blank";
  if is_reserved_tag_slug slug then invalid_arg "Tag name is reserved";
  [%platform
    match () with
    | Server ->
        let description = option_of_non_empty description in
        ignore (query_lines (insert_tag_sql ~slug ~name ~description));
        slug
    | Client -> slug]

let create_tag_from_form_data formData =
  [%platform
    match () with
    | Server ->
        let get_string name =
          Js.FormData.get formData name |> function
          | `String value -> value |> String.trim
        in
        let slug =
          create_or_lookup_tag ~name:(get_string "name")
            ~description:(get_string "description")
        in
        tag_route slug
    | Client ->
        let _ = formData in
        notes_route]

let create_note_from_form_data formData =
  [%platform
    match () with
    | Server ->
        let get_string name =
          Js.FormData.get formData name |> function
          | `String value -> value |> String.trim
        in
        let tag_slug = get_string "tag_slug" in
        if tag_slug = "" then invalid_arg "Select an existing tag";
        if Option.is_none (tag_summary_opt tag_slug) then
          invalid_arg "Selected tag does not exist";
        let title =
          match get_string "title" with "" -> "Untitled Note" | value -> value
        in
        let body_markdown =
          Js.FormData.get formData "body_markdown" |> function
          | `String value -> normalize_body_markdown value
        in
        let checklist =
          get_string "checklist" |> decode_checklist
          |> List.filter (fun item -> String.trim item.text <> "")
        in
        let note =
          make_note ~tag_slug
            ~slug:(timestamp_slug tag_slug title)
            ~title
            ~preview:(preview_of_body_markdown body_markdown)
            ~updated_at:"Just now" ~body_markdown ~checklist ~tags:[ tag_slug ]
            ()
        in
        ignore (query_lines (insert_sql note));
        note.summary.route
    | Client ->
        let _ = formData in
        notes_route]

let create_note_action ~tag_slug ~title ~body_markdown ~checklist_raw =
  [%platform
    match () with
    | Server ->
        let tag_slug = String.trim tag_slug in
        if tag_slug = "" then invalid_arg "Select an existing tag";
        if Option.is_none (tag_summary_opt tag_slug) then
          invalid_arg "Selected tag does not exist";
        let title =
          match String.trim title with "" -> "Untitled Note" | value -> value
        in
        let body_markdown = normalize_body_markdown body_markdown in
        let checklist =
          String.trim checklist_raw |> decode_checklist
          |> List.filter (fun item -> String.trim item.text <> "")
        in
        let note =
          make_note ~tag_slug
            ~slug:(timestamp_slug tag_slug title)
            ~title
            ~preview:(preview_of_body_markdown body_markdown)
            ~updated_at:"Just now" ~body_markdown ~checklist ~tags:[ tag_slug ]
            ()
        in
        ignore (query_lines (insert_sql note));
        note.summary.route
    | Client ->
        let _ = (tag_slug, title, body_markdown, checklist_raw) in
        notes_route]
