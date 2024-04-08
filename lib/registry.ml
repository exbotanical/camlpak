let registry_url = Util.get_env_var "NPM_REGISTRY" "https://registry.npmjs.org/"

open Lwt
open Cohttp_lwt_unix

let fetch_data (url : string) : string Lwt.t =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body

let get_manifest_from_registry (name : string) =
  let payload = Lwt_main.run (fetch_data (registry_url ^ name)) in

  Json.dep_entry_list_of_json payload
