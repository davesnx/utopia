type command = {
  name : string;
  aliases : string list;
  description : string;
  run : string list -> int;
}

let commands ~version =
  [
    {
      name = "build";
      aliases = [];
      description = "Build production artifacts";
      run = Build.run;
    };
    {
      name = "dev";
      aliases = [];
      description = "Start development workflow";
      run = Dev.run;
    };
    {
      name = "prod";
      aliases = [ "start" ];
      description = "Run built server in production mode";
      run = Prod.run;
    };
    {
      name = "clean";
      aliases = [];
      description = "Remove generated artifacts";
      run = Clean.run;
    };
    {
      name = "info";
      aliases = [];
      description = "Print environment and project metadata";
      run = Info.run ~version;
    };
  ]

let basename path =
  let len = String.length path in
  let rec scan index =
    if index < 0 then path
    else if path.[index] = '/' then String.sub path (index + 1) (len - index - 1)
    else scan (index - 1)
  in
  scan (len - 1)

let find_command commands name =
  let lower = String.lowercase_ascii name in
  List.find_opt
    (fun command ->
      command.name = lower
      || List.exists (fun alias -> alias = lower) command.aliases)
    commands

let print_usage commands =
  Printf.printf "%s\n\n" (Terminal.bold "utopia <command> [options]");
  Printf.printf "Commands:\n";
  List.iter
    (fun command ->
      let alias_suffix =
        match command.aliases with
        | [] -> ""
        | values ->
            Printf.sprintf " %s"
              (Terminal.dim
                 (Printf.sprintf "(alias: %s)" (String.concat ", " values)))
      in
      Printf.printf "  %-10s %s%s\n"
        (Terminal.cyan command.name)
        command.description alias_suffix)
    commands;
  Printf.printf "\nShortcuts work as executable aliases, e.g. %s.\n"
    (Terminal.cyan "utopia-build");
  Printf.printf "\nGlobal flags:\n";
  Printf.printf "  %s      Show this help\n" (Terminal.cyan "-h, --help");
  Printf.printf "  %s   Print CLI version\n" (Terminal.cyan "-v, --version")

let command_from_executable commands argv0 =
  let name = basename argv0 in
  if String.length name > 7 && String.sub name 0 7 = "utopia-" then
    let suffix = String.sub name 7 (String.length name - 7) in
    find_command commands suffix
  else None

let run ~version () =
  let commands = commands ~version in
  let argv = Array.to_list Sys.argv in
  let argv0 = List.hd argv in
  let executable_command = command_from_executable commands argv0 in
  match List.tl argv with
  | [ "-h" ] | [ "--help" ] ->
      print_usage commands;
      0
  | [ "-v" ] | [ "--version" ] ->
      Printf.printf "%s\n" version;
      0
  | args -> (
      match (executable_command, args) with
      | Some command, remaining -> command.run remaining
      | None, [] ->
          let dev =
            match find_command commands "dev" with
            | Some command -> command
            | None -> failwith "dev command missing"
          in
          dev.run []
      | None, command_name :: command_args -> (
          match find_command commands command_name with
          | Some command -> command.run command_args
          | None ->
              Printf.eprintf "Unknown command: %s\n\n%!" command_name;
              print_usage commands;
              1))
