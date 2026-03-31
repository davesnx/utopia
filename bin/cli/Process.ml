let env_name binding =
  match String.index_opt binding '=' with
  | Some index -> String.sub binding 0 index
  | None -> binding

let should_strip_child_env name =
  match name with
  | "INSIDE_DUNE" | "DUNE_SOURCEROOT" | "DUNE_OCAML_STDLIB"
  | "DUNE_OCAML_HARDCODED" ->
      true
  | _ -> false

let child_env ?(extra = [||]) () =
  let base =
    Unix.environment () |> Array.to_list
    |> List.filter (fun binding ->
        binding |> env_name |> should_strip_child_env |> not)
    |> Array.of_list
  in
  Array.append base extra

let unix_status_code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal -> 128 + signal
  | Unix.WSTOPPED signal -> 128 + signal

let file_mtime path =
  try Some (Unix.stat path).Unix.st_mtime with Unix.Unix_error _ -> None

let max_port = 65535
let valid_port port = port >= 1 && port <= max_port

let parse_port value =
  match int_of_string_opt value with
  | Some port when valid_port port -> Ok port
  | _ -> Error (Printf.sprintf "Invalid PORT value '%s'" value)

let resolve_inet_addr host =
  try Ok (Unix.inet_addr_of_string host)
  with Failure _ -> (
    try
      let entry = Unix.gethostbyname host in
      if Array.length entry.Unix.h_addr_list = 0 then
        Error (Printf.sprintf "Could not resolve HOST value '%s'" host)
      else Ok entry.Unix.h_addr_list.(0)
    with Not_found | Unix.Unix_error _ ->
      Error (Printf.sprintf "Could not resolve HOST value '%s'" host))

let probe_port_availability ~inet_addr ~port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      Unix.setsockopt socket Unix.SO_REUSEADDR true;
      Unix.bind socket (Unix.ADDR_INET (inet_addr, port));
      true)

let port_is_available ~inet_addr ~port =
  try probe_port_availability ~inet_addr ~port
  with
  | Unix.Unix_error (Unix.EADDRINUSE, _, _) | Unix.Unix_error (Unix.EACCES, _, _)
  ->
    false

let first_available_port ~host port =
  if not (valid_port port) then
    Error (Printf.sprintf "Port %d must be between 1 and %d" port max_port)
  else
    match resolve_inet_addr host with
    | Error _ as error -> error
    | Ok inet_addr ->
        let rec loop candidate =
          if candidate > max_port then
            Error
              (Printf.sprintf
                 "Could not find an available port on %s starting from %d" host
                 port)
          else if port_is_available ~inet_addr ~port:candidate then Ok candidate
          else loop (candidate + 1)
        in
        loop port

let run_command program args =
  let pid =
    Unix.create_process_env program
      (Array.of_list (program :: args))
      (child_env ()) Unix.stdin Unix.stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED _ -> unix_status_code status
  | Unix.WSIGNALED signal ->
      Printf.eprintf "Process %s killed by signal %d\n%!" program signal;
      unix_status_code status
  | Unix.WSTOPPED signal ->
      Printf.eprintf "Process %s stopped by signal %d\n%!" program signal;
      unix_status_code status

let run_command_capture program args =
  try
    let read_end, write_end = Unix.pipe () in
    let pid =
      Unix.create_process_env program
        (Array.of_list (program :: args))
        (child_env ()) Unix.stdin write_end write_end
    in
    Unix.close write_end;
    let channel = Unix.in_channel_of_descr read_end in
    let buffer = Buffer.create 256 in
    (try
       while true do
         Buffer.add_char buffer (input_char channel)
       done
     with End_of_file -> ());
    close_in channel;
    let _, status = Unix.waitpid [] pid in
    let output = Buffer.contents buffer |> String.trim in
    match status with Unix.WEXITED 0 -> Some output | _ -> None
  with Unix.Unix_error _ -> None

let spawn program args env =
  Unix.create_process_env program
    (Array.of_list (program :: args))
    env Unix.stdin Unix.stdout Unix.stderr

let spawn_silent program args env =
  let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
  let pid =
    Unix.create_process_env program
      (Array.of_list (program :: args))
      env Unix.stdin dev_null dev_null
  in
  Unix.close dev_null;
  pid

let kill_if_alive pid =
  try Unix.kill pid Sys.sigterm with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
