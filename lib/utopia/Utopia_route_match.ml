(* Route matching primitives shared between compiler, server, and benchmarks. *)

open Utopia_types

type param_value = One of string | Many of string list

let parse_matcher_segment segment =
  if String.length segment >= 2 && String.sub segment 0 2 = "**" then
    Ok
      (Param
         (String.sub segment 2 (String.length segment - 2), Optional_catch_all))
  else if String.length segment >= 1 && segment.[0] = '*' then
    Ok (Param (String.sub segment 1 (String.length segment - 1), Catch_all))
  else if String.length segment >= 1 && segment.[0] = ':' then
    Ok (Param (String.sub segment 1 (String.length segment - 1), Single))
  else Ok (Static segment)

let parse_matcher matcher =
  if matcher = "" then Ok []
  else
    matcher |> String.split_on_char '/'
    |> List.fold_left
         (fun acc segment ->
           match (acc, parse_matcher_segment segment) with
           | (Error _ as error), _ -> error
           | Ok _, Error message -> Error message
           | Ok segments, Ok parsed_segment -> Ok (parsed_segment :: segments))
         (Ok [])
    |> Result.map List.rev

let specificity_of_segment = function
  | Static _ -> 4
  | Param (_, Single) -> 3
  | Param (_, Catch_all) -> 2
  | Param (_, Optional_catch_all) -> 1

let compare_specificity left_segments right_segments =
  let rec compare_scores left_scores right_scores =
    match (left_scores, right_scores) with
    | [], [] -> 0
    | _ :: _, [] -> -1
    | [], _ :: _ -> 1
    | left_score :: left_rest, right_score :: right_rest ->
        if left_score > right_score then -1
        else if left_score < right_score then 1
        else compare_scores left_rest right_rest
  in
  compare_scores
    (List.map specificity_of_segment left_segments)
    (List.map specificity_of_segment right_segments)

let normalize_target target =
  let without_query =
    match String.index_opt target '?' with
    | Some index -> String.sub target 0 index
    | None -> target
  in
  if without_query = "/" then ""
  else if String.length without_query > 0 && without_query.[0] = '/' then
    String.sub without_query 1 (String.length without_query - 1)
  else without_query

let target_segments target =
  if target = "" then []
  else
    target |> String.split_on_char '/'
    |> List.filter (fun segment -> segment <> "")

let strip_query_and_hash path =
  let limit = ref (String.length path) in
  (match String.index_opt path '?' with
  | Some index -> limit := min !limit index
  | None -> ());
  (match String.index_opt path '#' with
  | Some index -> limit := min !limit index
  | None -> ());
  String.sub path 0 !limit

let path_segments path =
  strip_query_and_hash path |> String.split_on_char '/'
  |> List.filter (fun segment -> segment <> "")

let render_matcher_segment = function
  | Static segment -> String.lowercase_ascii segment
  | Param (name, Single) -> ":" ^ name
  | Param (name, Catch_all) -> "*" ^ name
  | Param (name, Optional_catch_all) -> "**" ^ name

let route_definition_of_segments segments =
  match List.map render_matcher_segment segments with
  | [] -> "/"
  | parts -> "/" ^ String.concat "/" parts

let common_prefix_length left right =
  let rec loop count left right =
    match (left, right) with
    | left_head :: left_tail, right_head :: right_tail
      when left_head = right_head ->
        loop (count + 1) left_tail right_tail
    | _ -> count
  in
  loop 0 left right

let rec match_segments route_segments path_segments params =
  match (route_segments, path_segments) with
  | [], [] -> Some (List.rev params)
  | Static expected :: rest_route, current :: rest_path
    when expected = String.lowercase_ascii current ->
      match_segments rest_route rest_path params
  | Param (name, Single) :: rest_route, current :: rest_path ->
      match_segments rest_route rest_path ((name, One current) :: params)
  | [ Param (name, Catch_all) ], rest_path ->
      if rest_path = [] then None
      else Some (List.rev ((name, Many rest_path) :: params))
  | [ Param (name, Optional_catch_all) ], rest_path ->
      Some (List.rev ((name, Many rest_path) :: params))
  | _ -> None
