type destination = Root_only | Native_only | Both

type file = {
  target_name : string;
  module_name : string;
  repository_source_path : string;
  installed_source_path : string;
  destination : destination;
}

let files_directory_name = "utopia_runtime"

let make_custom_file ?(destination = Both) ~target_name ~repository_source_path
    ~installed_source_path () =
  {
    target_name;
    module_name = Filename.remove_extension target_name;
    repository_source_path;
    installed_source_path;
    destination;
  }

let make_file ?(destination = Both) target_name =
  make_custom_file ~destination ~target_name
    ~repository_source_path:
      (Filename.concat "lib"
         (Filename.concat files_directory_name
            (Filename.concat "files" target_name)))
    ~installed_source_path:(Filename.concat files_directory_name target_name)
    ()

let react_server_dom_esbuild =
  make_file ~destination:Root_only "ReactServerDOMEsbuild.re"

let function_references =
  make_file ~destination:Native_only "FunctionReferences.re"

let utopia = make_file "Utopia.re"
let utopia_route = make_file "Utopia_route.ml"
let utopia_router = make_file "Utopia_router.re"
let utopia_router_route = make_file "Utopia_router_route.re"
let utopia_router_link = make_file "Utopia_router_link.re"

let utopia_route_builder =
  make_file ~destination:Native_only "Utopia_route_builder.ml"

let esbuild_config = make_file ~destination:Root_only "esbuild.config.mjs"

let client_entry_source_file =
  make_file ~destination:Root_only "client_entry.re"

let utopia_server_source_file =
  make_custom_file ~destination:Root_only ~target_name:"Utopia_server.ml"
    ~repository_source_path:"lib/server/server.ml"
    ~installed_source_path:"utopia_runtime/Utopia_server.ml" ()

let utopia_types_source_file =
  make_custom_file ~destination:Both ~target_name:"Utopia_types.ml"
    ~repository_source_path:"lib/utopia_types/utopia_types.ml"
    ~installed_source_path:"utopia_runtime/Utopia_types.ml" ()

let all_files =
  [
    react_server_dom_esbuild;
    function_references;
    utopia;
    utopia_route;
    utopia_router;
    utopia_router_route;
    utopia_router_link;
    esbuild_config;
    client_entry_source_file;
    utopia_route_builder;
    utopia_server_source_file;
    utopia_types_source_file;
  ]

let include_in_root file =
  match file.destination with Root_only | Both -> true | Native_only -> false

let include_in_native file =
  match file.destination with Native_only | Both -> true | Root_only -> false

let root_files = List.filter include_in_root all_files
let native_files = List.filter include_in_native all_files

let melange_module_names =
  [
    utopia.module_name;
    utopia_route.module_name;
    utopia_types_source_file.module_name;
    react_server_dom_esbuild.module_name;
    utopia_router.module_name;
    utopia_router_route.module_name;
    utopia_router_link.module_name;
  ]

let native_module_names =
  [
    function_references.module_name;
    utopia.module_name;
    utopia_route.module_name;
    utopia_types_source_file.module_name;
    utopia_router.module_name;
    utopia_router_route.module_name;
    utopia_router_link.module_name;
    utopia_route_builder.module_name;
  ]

let target_name file = file.target_name
let module_name file = file.module_name
let repository_source_path file = file.repository_source_path
let installed_source_path file = file.installed_source_path
let client_entry_melange_target_name = "client_entry_melange.re"
let client_entry_melange_module_name = "client_entry_melange"
