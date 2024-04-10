let registry_url = Util.get_env_var "NPM_REGISTRY" "https://registry.npmjs.org/"

open Lwt
open Cohttp
open Cohttp_lwt_unix

exception DependencyNotFound of string

module type Registry = sig
  val get : string -> string Lwt.t
end

module NpmRegistry : Registry = struct
  let get (url : string) : string Lwt.t =
    Client.get (Uri.of_string url) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in

    match code with
    | 200 -> Cohttp_lwt.Body.to_string body
    | _ -> raise (DependencyNotFound url)
end

let get_manifest_from_registry (name : string) (module R : Registry) =
  let payload = Lwt_main.run (R.get (registry_url ^ name)) in

  Json.dep_entry_list_of_json payload
