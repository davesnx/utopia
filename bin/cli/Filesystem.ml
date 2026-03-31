let file_exists path = Sys.file_exists path
let is_directory path = Sys.file_exists path && Sys.is_directory path

let rec remove_recursive path =
  if Sys.is_directory path then (
    Sys.readdir path
    |> Array.iter (fun entry -> remove_recursive (Filename.concat path entry));
    Unix.rmdir path)
  else Sys.remove path

let remove_if_exists path =
  if file_exists path then (
    (try remove_recursive path
     with exn ->
       Terminal.print_warn
         (Printf.sprintf "Could not fully remove %s: %s" path
            (Printexc.to_string exn)));
    true)
  else false
