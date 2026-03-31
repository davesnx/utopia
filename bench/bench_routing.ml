(* Micro-benchmarks for the server's routing hot path.

   Tests route matching, segment parsing, HTML rendering, and manifest loading
   with varying numbers of routes to catch performance regressions.

   Run with: dune exec bench/bench_routing.exe *)

(* -------------------------------------------------------------------------- *)
(* Types intentionally duplicated to keep the benchmark self-contained.        *)
(* The shared library exists, but benchmarks stay isolated to avoid drift in   *)
(* startup/linking behavior from affecting the measurements.                   *)
(* -------------------------------------------------------------------------- *)

type page_kind = Code_page | Markdown_page
type param_kind = Single | Catch_all | Optional_catch_all
type route_segment = Static of string | Param of string * param_kind
type param_value = One of string | Many of string list

type route_entry = {
  route : string;
  params : (string * param_kind) list;
  layouts : string list;
  kind : page_kind;
  source_file : string;
  segments : route_segment list;
}

(* -------------------------------------------------------------------------- *)
(* Functions under test (mirrored from server.ml)                             *)
(* -------------------------------------------------------------------------- *)

let normalize_target target =
  if target = "/" then ""
  else if String.length target > 0 && target.[0] = '/' then
    String.sub target 1 (String.length target - 1)
  else target

let target_segments target =
  if target = "" then []
  else target |> String.split_on_char '/' |> List.filter (fun s -> s <> "")

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

let find_match routes path_segments =
  routes
  |> List.find_map (fun route ->
      match match_segments route.segments path_segments [] with
      | None -> None
      | Some params -> Some (route, params))

let escape_html text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&#39;"
      | c -> Buffer.add_char buffer c)
    text;
  Buffer.contents buffer

let render_param_value = function
  | One value -> value
  | Many values -> String.concat "/" values

let render_params params =
  if params = [] then ""
  else
    params
    |> List.map (fun (name, value) ->
        Printf.sprintf "<li><code>%s</code> = %s</li>" (escape_html name)
          (escape_html (render_param_value value)))
    |> String.concat ""
    |> Printf.sprintf "<h2>Params</h2><ul>%s</ul>"

let wrap_with_layouts layouts content =
  List.fold_right
    (fun layout acc ->
      Printf.sprintf "<section><div>Layout: <code>%s</code></div>%s</section>"
        (escape_html layout) acc)
    layouts content

let html_page ~title ~body =
  Printf.sprintf
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><meta \
     name=\"viewport\" content=\"width=device-width, \
     initial-scale=1\"><title>%s</title></head><body>%s</body></html>"
    (escape_html title) body

let render_code_page route source_content params layouts =
  let source = escape_html source_content in
  let content =
    Printf.sprintf
      "<main><h1>/%s</h1><p>Code page from \
       <code>%s</code>.</p>%s<pre>%s</pre></main>"
      (escape_html route) (escape_html route) (render_params params) source
  in
  html_page ~title:route ~body:(wrap_with_layouts layouts content)

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

let compare_route_specificity left right =
  let rec compare_scores left_scores right_scores =
    match (left_scores, right_scores) with
    | [], [] -> 0
    | _ :: _, [] -> -1
    | [], _ :: _ -> 1
    | ls :: lr, rs :: rr ->
        if ls > rs then -1 else if ls < rs then 1 else compare_scores lr rr
  in
  let left_scores = List.map specificity_of_segment left.segments in
  let right_scores = List.map specificity_of_segment right.segments in
  compare_scores left_scores right_scores

(* -------------------------------------------------------------------------- *)
(* Benchmark harness                                                          *)
(* -------------------------------------------------------------------------- *)

type bench_result = {
  name : string;
  iterations : int;
  total_ns : float;
  mean_ns : float;
  min_ns : float;
  max_ns : float;
  median_ns : float;
  p99_ns : float;
  ops_per_sec : float;
}

(* We batch multiple calls per sample to amortize gettimeofday overhead.
   Each sample runs the function [batch_size] times, then we divide to get
   the per-call time. *)

let percentile sorted_array p =
  let n = Array.length sorted_array in
  if n = 0 then 0.0
  else
    let index = Float.to_int (Float.round (Float.of_int (n - 1) *. p)) in
    sorted_array.(min index (n - 1))

let run_bench ~name ~warmup ~iterations f =
  let batch_size = 100 in
  (* Warmup *)
  for _ = 1 to warmup do
    ignore (f ())
  done;
  (* Collect samples: each sample is [batch_size] calls *)
  let num_samples = iterations / batch_size in
  let num_samples = max num_samples 100 in
  let samples =
    Array.init num_samples (fun _ ->
        let t0 = Unix.gettimeofday () in
        for _ = 1 to batch_size do
          ignore (f ())
        done;
        let t1 = Unix.gettimeofday () in
        (t1 -. t0) *. 1_000_000_000.0 /. Float.of_int batch_size)
  in
  let sorted = Array.copy samples in
  Array.sort Float.compare sorted;
  let total = Array.fold_left ( +. ) 0.0 samples in
  let mean = total /. Float.of_int num_samples in
  {
    name;
    iterations = num_samples * batch_size;
    total_ns = total *. Float.of_int batch_size;
    mean_ns = mean;
    min_ns = sorted.(0);
    max_ns = sorted.(Array.length sorted - 1);
    median_ns = percentile sorted 0.5;
    p99_ns = percentile sorted 0.99;
    ops_per_sec = 1_000_000_000.0 /. mean;
  }

let format_ns ns =
  if ns >= 1_000_000_000.0 then Printf.sprintf "%.2f s" (ns /. 1_000_000_000.0)
  else if ns >= 1_000_000.0 then Printf.sprintf "%.2f ms" (ns /. 1_000_000.0)
  else if ns >= 1_000.0 then Printf.sprintf "%.2f us" (ns /. 1_000.0)
  else Printf.sprintf "%.0f ns" ns

let format_ops ops =
  if ops >= 1_000_000.0 then Printf.sprintf "%.2fM" (ops /. 1_000_000.0)
  else if ops >= 1_000.0 then Printf.sprintf "%.2fK" (ops /. 1_000.0)
  else Printf.sprintf "%.0f" ops

let print_result r =
  Printf.printf "  %-45s  mean=%s  median=%s  p99=%s  ops/s=%s\n" r.name
    (format_ns r.mean_ns) (format_ns r.median_ns) (format_ns r.p99_ns)
    (format_ops r.ops_per_sec)

(* -------------------------------------------------------------------------- *)
(* Route generators                                                           *)
(* -------------------------------------------------------------------------- *)

let make_static_route path =
  let segments =
    match parse_matcher path with
    | Ok s -> s
    | Error _ -> failwith "bad matcher"
  in
  {
    route = path;
    params = [];
    layouts = [];
    kind = Code_page;
    source_file = Printf.sprintf "pages/%s.re" path;
    segments;
  }

let make_param_route path =
  let segments =
    match parse_matcher path with
    | Ok s -> s
    | Error _ -> failwith "bad matcher"
  in
  let params =
    segments
    |> List.filter_map (function
      | Param (name, kind) -> Some (name, kind)
      | Static _ -> None)
  in
  {
    route = path;
    params;
    layouts = [];
    kind = Code_page;
    source_file = Printf.sprintf "pages/%s.re" path;
    segments;
  }

let generate_routes n =
  let routes = ref [] in
  for i = 0 to n - 1 do
    let route =
      match i mod 5 with
      | 0 -> make_static_route (Printf.sprintf "section%d/page%d" (i / 10) i)
      | 1 -> make_param_route (Printf.sprintf "section%d/:id" (i / 10))
      | 2 ->
          make_static_route
            (Printf.sprintf "section%d/page%d/detail" (i / 10) i)
      | 3 -> make_param_route (Printf.sprintf "api/v%d/:resource/:id" (i / 10))
      | _ -> make_static_route (Printf.sprintf "page%d" i)
    in
    routes := route :: !routes
  done;
  List.rev !routes |> List.sort compare_route_specificity

let sample_source_content =
  "let page = () => {\n\
  \  <div>\n\
  \    <h1>{React.string(\"Hello World\")}</h1>\n\
  \    <p>{React.string(\"This is a sample page with some content.\")}</p>\n\
  \    <ul>\n\
  \      {[1, 2, 3, 4, 5]\n\
  \       |> List.map(i => <li key={string_of_int(i)}>{React.int(i)}</li>)\n\
  \       |> React.list}\n\
  \    </ul>\n\
  \  </div>;\n\
   };\n"

(* -------------------------------------------------------------------------- *)
(* Benchmark definitions                                                      *)
(* -------------------------------------------------------------------------- *)

let bench_normalize_target () =
  run_bench ~name:"normalize_target" ~warmup:1000 ~iterations:100_000 (fun () ->
      ignore (normalize_target "/");
      ignore (normalize_target "/about");
      ignore (normalize_target "/api/users/123/posts");
      ignore (normalize_target "already-normalized"))

let bench_target_segments () =
  run_bench ~name:"target_segments" ~warmup:1000 ~iterations:100_000 (fun () ->
      ignore (target_segments "");
      ignore (target_segments "about");
      ignore (target_segments "api/users/123/posts");
      ignore (target_segments "a/b/c/d/e/f/g/h"))

let bench_match_segments_static () =
  let route_segments = [ Static "api"; Static "users"; Static "list" ] in
  let path_hit = [ "api"; "users"; "list" ] in
  let path_miss = [ "api"; "posts"; "list" ] in
  let r1 =
    run_bench ~name:"match_segments (static hit)" ~warmup:1000
      ~iterations:100_000 (fun () ->
        ignore (match_segments route_segments path_hit []))
  in
  let r2 =
    run_bench ~name:"match_segments (static miss)" ~warmup:1000
      ~iterations:100_000 (fun () ->
        ignore (match_segments route_segments path_miss []))
  in
  (r1, r2)

let bench_match_segments_params () =
  let route_segments =
    [ Static "api"; Static "users"; Param ("id", Single); Static "posts" ]
  in
  let path = [ "api"; "users"; "42"; "posts" ] in
  run_bench ~name:"match_segments (with params)" ~warmup:1000
    ~iterations:100_000 (fun () ->
      ignore (match_segments route_segments path []))

let bench_match_segments_catch_all () =
  let route_segments = [ Static "docs"; Param ("path", Catch_all) ] in
  let path = [ "docs"; "api"; "reference"; "server"; "routing" ] in
  run_bench ~name:"match_segments (catch-all)" ~warmup:1000 ~iterations:100_000
    (fun () -> ignore (match_segments route_segments path []))

let bench_find_match ~label routes target =
  let segments = target_segments (normalize_target target) in
  run_bench ~name:label ~warmup:1000 ~iterations:50_000 (fun () ->
      ignore (find_match routes segments))

let bench_render_code_page () =
  let params = [ ("id", One "42"); ("slug", One "hello-world") ] in
  let layouts = [ "pages/layout.re"; "pages/blog/layout.re" ] in
  run_bench ~name:"render_code_page" ~warmup:500 ~iterations:10_000 (fun () ->
      ignore
        (render_code_page "blog/posts/42" sample_source_content params layouts))

let bench_render_code_page_no_params () =
  run_bench ~name:"render_code_page (no params)" ~warmup:500 ~iterations:10_000
    (fun () -> ignore (render_code_page "about" sample_source_content [] []))

let bench_escape_html () =
  let text_clean = "Hello World! This is a normal paragraph of text." in
  let text_dirty =
    "<script>alert('xss')</script> & \"quotes\" & 'apostrophes' <b>bold</b>"
  in
  let r1 =
    run_bench ~name:"escape_html (clean)" ~warmup:1000 ~iterations:100_000
      (fun () -> ignore (escape_html text_clean))
  in
  let r2 =
    run_bench ~name:"escape_html (with special chars)" ~warmup:1000
      ~iterations:100_000 (fun () -> ignore (escape_html text_dirty))
  in
  (r1, r2)

let bench_parse_matcher () =
  let r1 =
    run_bench ~name:"parse_matcher (static)" ~warmup:1000 ~iterations:50_000
      (fun () -> ignore (parse_matcher "api/users/list"))
  in
  let r2 =
    run_bench ~name:"parse_matcher (params)" ~warmup:1000 ~iterations:50_000
      (fun () -> ignore (parse_matcher "api/:version/users/:id"))
  in
  (r1, r2)

(* -------------------------------------------------------------------------- *)
(* Main                                                                       *)
(* -------------------------------------------------------------------------- *)

let () =
  let routes_10 = generate_routes 10 in
  let routes_50 = generate_routes 50 in
  let routes_100 = generate_routes 100 in
  let routes_500 = generate_routes 500 in

  Printf.printf "\n  Utopia Server Benchmarks\n";
  Printf.printf "  %s\n\n" (String.make 72 '-');

  Printf.printf "  String operations:\n";
  print_result (bench_normalize_target ());
  print_result (bench_target_segments ());
  let r1, r2 = bench_escape_html () in
  print_result r1;
  print_result r2;

  Printf.printf "\n  Matcher parsing:\n";
  let r1, r2 = bench_parse_matcher () in
  print_result r1;
  print_result r2;

  Printf.printf "\n  Segment matching:\n";
  let r1, r2 = bench_match_segments_static () in
  print_result r1;
  print_result r2;
  print_result (bench_match_segments_params ());
  print_result (bench_match_segments_catch_all ());

  Printf.printf "\n  Route lookup (find_match) - scaling with route count:\n";
  (* Hit: target that matches early in the route table *)
  print_result
    (bench_find_match ~label:"find_match (10 routes, early hit)" routes_10
       "/page4");
  print_result
    (bench_find_match ~label:"find_match (50 routes, early hit)" routes_50
       "/page4");
  print_result
    (bench_find_match ~label:"find_match (100 routes, early hit)" routes_100
       "/page4");
  print_result
    (bench_find_match ~label:"find_match (500 routes, early hit)" routes_500
       "/page4");

  (* Hit: target that matches late in the route table *)
  print_result
    (bench_find_match ~label:"find_match (10 routes, late hit)" routes_10
       "/section0/page5/detail");
  print_result
    (bench_find_match ~label:"find_match (50 routes, late hit)" routes_50
       "/section4/page47/detail");
  print_result
    (bench_find_match ~label:"find_match (100 routes, late hit)" routes_100
       "/section9/page97/detail");
  print_result
    (bench_find_match ~label:"find_match (500 routes, late hit)" routes_500
       "/section49/page497/detail");

  (* Miss: no route matches *)
  print_result
    (bench_find_match ~label:"find_match (10 routes, miss)" routes_10
       "/nonexistent/path");
  print_result
    (bench_find_match ~label:"find_match (100 routes, miss)" routes_100
       "/nonexistent/path");
  print_result
    (bench_find_match ~label:"find_match (500 routes, miss)" routes_500
       "/nonexistent/path");

  Printf.printf "\n  HTML rendering:\n";
  print_result (bench_render_code_page ());
  print_result (bench_render_code_page_no_params ());

  Printf.printf "\n  %s\n" (String.make 72 '-');
  Printf.printf "  Done.\n\n"
