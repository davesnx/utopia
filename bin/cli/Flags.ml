type dev = { port : string; host : string; no_watch : bool; verbose : bool }
type clean_mode = Full | Build_outputs
type clean = { mode : clean_mode }

let default_dev =
  {
    port =
      (match Sys.getenv_opt "PORT" with Some port -> port | None -> "8080");
    host =
      (match Sys.getenv_opt "HOST" with
      | Some host -> host
      | None -> "127.0.0.1");
    no_watch = false;
    verbose = false;
  }

let default_clean = { mode = Full }

let parse_dev args =
  let rec loop config = function
    | [] -> config
    | "--port" :: value :: rest -> loop { config with port = value } rest
    | "--host" :: value :: rest -> loop { config with host = value } rest
    | "--no-watch" :: rest -> loop { config with no_watch = true } rest
    | "--verbose" :: rest -> loop { config with verbose = true } rest
    | unknown :: _ ->
        Printf.eprintf "Unknown dev flag: %s\n%!" unknown;
        exit 1
  in
  loop default_dev args

let parse_clean args =
  let rec loop config = function
    | [] -> config
    | "--build-outputs" :: rest -> loop { mode = Build_outputs } rest
    | unknown :: _ ->
        Printf.eprintf "Unknown clean flag: %s\n%!" unknown;
        exit 1
  in
  loop default_clean args
