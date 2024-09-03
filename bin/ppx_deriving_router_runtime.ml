module Routerino = struct
  type request
  type response
  type status = [ `OK | `Bad_Request | `Method_Not_Allowed | `Not_Found ]

  let queries _ = []
  let body _ = Lwt.return ""
  let target _ = ""
  let method_ _ = `GET

  let respond ~(status : status) ~(headers : (string * string) list)
      (body : string) =
    Lwt.return ""

  let status ~(headers : (string * string) list) (body : string) = Lwt.return ""
end

open struct
  module Request :
    Ppx_deriving_router_runtime_lib.REQUEST with type t = Routerino.request =
  struct
    type t = Routerino.request

    let queries = Routerino.queries
    let body = Routerino.body
    let path = Routerino.target

    let method_ req =
      match Routerino.method_ req with
      | `GET -> `GET
      | `POST -> `POST
      | `PUT -> `PUT
      | `DELETE -> `DELETE
      | _ -> failwith "Unsupported method"
  end

  module Response :
    Ppx_deriving_router_runtime_lib.RESPONSE
      with type status = Routerino.status
       and type t = Routerino.response = struct
    type status = Routerino.status

    let status_ok : status = `OK
    let status_bad_request : status = `Bad_Request
    let status_method_not_allowed : status = `Method_Not_Allowed
    let status_not_found : status = `Not_Found

    type t = Routerino.response

    let respond ~status ~headers body = Routerino.respond ~status ~headers body
  end

  module Return :
    Ppx_deriving_router_runtime_lib.RETURN
      with type status = Routerino.status
       and type 'a t = 'a = struct
    type status = Routerino.status
    type 'a t = 'a

    let data x = Some x
    let status _ = None
    let headers _ = []
  end
end

include Ppx_deriving_router_runtime_lib.Make (Request) (Response) (Return)
