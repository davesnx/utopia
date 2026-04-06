type t = Hashtbl.t(string, ReactServerDOM.server_function);

let registry: t = Hashtbl.create(32);
let register = Hashtbl.replace(registry);
let get = Hashtbl.find_opt(registry);
