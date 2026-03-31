open Melange_json.Primitives

module Query = struct
  type t = (string * string) list

  let empty = []
end

module Params = struct
  type value = One of string | Many of string list
  type t = (string * value) list

  let one value = One value
  let many values = Many values

  let find name params =
    params
    |> List.find_map (fun (candidate, value) ->
        if String.equal candidate name then Some value else None)

  let find_one name params =
    match find name params with Some (One value) -> Some value | _ -> None

  let find_many name params =
    match find name params with Some (Many values) -> Some values | _ -> None

  let segments_exn ~route ~name ~kind params =
    match (kind, find name params) with
    | Utopia_types.Single, Some (One value) -> [ value ]
    | Utopia_types.Catch_all, Some (Many values) ->
        if values = [] then
          invalid_arg
            (Printf.sprintf
               "Invalid encoded path params for %s: %s must contain at least \
                one segment"
               route name)
        else values
    | Utopia_types.Optional_catch_all, None -> []
    | Utopia_types.Optional_catch_all, Some (Many values) -> values
    | Utopia_types.Single, Some (Many _)
    | Utopia_types.Catch_all, Some (One _)
    | Utopia_types.Optional_catch_all, Some (One _) ->
        invalid_arg
          (Printf.sprintf
             "Invalid encoded path params for %s: %s used the wrong segment \
              shape"
             route name)
    | Utopia_types.Single, None | Utopia_types.Catch_all, None ->
        invalid_arg
          (Printf.sprintf "Invalid encoded path params for %s: missing %s" route
             name)
end

module Hash = struct
  type t = string
end

module Nonempty = struct
  type 'a t = { head : 'a; tail : 'a list }

  let make ?(tail = []) ~head () = { head; tail }
  let to_list { head; tail } = head :: tail
  let of_list = function [] -> None | head :: tail -> Some { head; tail }
end

type t = { pathname : string; request_path : string; href : string }
[@@deriving json]

let split_once value separator =
  match String.index_opt value separator with
  | None -> (value, None)
  | Some index ->
      let left = String.sub value 0 index in
      let right =
        String.sub value (index + 1) (String.length value - index - 1)
      in
      (left, Some right)

let strip_query_and_hash value =
  let value, _hash = split_once value '#' in
  let value, _query = split_once value '?' in
  value

let normalize_pathname value =
  let raw = value |> strip_query_and_hash |> String.trim in
  let raw =
    if raw = "" then "/" else if raw.[0] = '/' then raw else "/" ^ raw
  in
  let rec trim_trailing_slashes value =
    if String.length value <= 1 || value.[String.length value - 1] <> '/' then
      value
    else trim_trailing_slashes (String.sub value 0 (String.length value - 1))
  in
  trim_trailing_slashes raw

let is_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let percent_encode value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (fun char ->
      if is_unreserved char then Buffer.add_char buffer char
      else Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code char)))
    value;
  Buffer.contents buffer

let hex_value = function
  | '0' .. '9' as char -> Char.code char - Char.code '0'
  | 'A' .. 'F' as char -> 10 + Char.code char - Char.code 'A'
  | 'a' .. 'f' as char -> 10 + Char.code char - Char.code 'a'
  | _ -> -1

let percent_decode value =
  let buffer = Buffer.create (String.length value) in
  let rec loop index =
    if index >= String.length value then ()
    else if value.[index] = '%' && index + 2 < String.length value then
      let hi = hex_value value.[index + 1] in
      let lo = hex_value value.[index + 2] in
      if hi >= 0 && lo >= 0 then (
        Buffer.add_char buffer (Char.chr ((hi * 16) + lo));
        loop (index + 3))
      else (
        Buffer.add_char buffer value.[index];
        loop (index + 1))
    else (
      Buffer.add_char buffer value.[index];
      loop (index + 1))
  in
  loop 0;
  Buffer.contents buffer

let render_pathname segments =
  match segments with
  | [] -> "/"
  | _ -> "/" ^ String.concat "/" (List.map percent_encode segments)

let render_query query =
  match query with
  | [] -> ""
  | entries ->
      let render_entry (key, value) =
        percent_encode key ^ "=" ^ percent_encode value
      in
      "?" ^ String.concat "&" (List.map render_entry entries)

let render_hash = function
  | None -> ""
  | Some value when String.trim value = "" -> ""
  | Some value -> "#" ^ percent_encode value

let make ~pathname ?(query = Query.empty) ?hash () =
  let pathname = normalize_pathname pathname in
  let request_path = pathname ^ render_query query in
  let href = request_path ^ render_hash hash in
  { pathname; request_path; href }

let from_segments ~segments ?query ?hash () =
  make ~pathname:(render_pathname segments) ?query ?hash ()

let of_href raw =
  let before_hash, hash = split_once raw '#' in
  let before_query, query = split_once before_hash '?' in
  let pathname = normalize_pathname before_query in
  let request_path =
    match query with
    | Some "" | None -> pathname
    | Some value -> pathname ^ "?" ^ value
  in
  let href =
    match hash with
    | Some "" | None -> request_path
    | Some value -> request_path ^ "#" ^ value
  in
  { pathname; request_path; href }

let path_segments route =
  route.pathname |> String.split_on_char '/'
  |> List.filter (fun segment -> segment <> "")
  |> List.map percent_decode

let parse_query_entries raw =
  if raw = "" then []
  else
    raw |> String.split_on_char '&'
    |> List.filter (fun piece -> piece <> "")
    |> List.map (fun entry ->
        let key, value = split_once entry '=' in
        (percent_decode key, value |> Option.value ~default:"" |> percent_decode))

let query_entries route =
  match split_once route.request_path '?' with
  | _pathname, None -> []
  | _pathname, Some query -> parse_query_entries query

let hash route =
  match split_once route.href '#' with
  | _href, None | _href, Some "" -> None
  | _href, Some value -> Some (percent_decode value)

let href route = route.href
let pathname route = route.pathname
let request_path route = route.request_path
let equal left right = String.equal left.href right.href
let same_pathname left right = String.equal left.pathname right.pathname

let same_request_path left right =
  String.equal left.request_path right.request_path
