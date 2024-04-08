type deps_stack_item = {
  name : string;
  version : string;
  dependencies : string * string;
}

type deps_stack = deps_stack_item list

(* TODO: *)
let max_satisfying (versions : string list) (v_constraint : string) =
  print_string v_constraint;
  List.length versions

let get_deps (name : string) (v_constraint : string) =
  let manifests =
    match Lock.get_dep name v_constraint with
    | Some dep -> dep
    | None -> Registry.get_manifest_from_registry name
  in

  let versions = List.map (fun (version, _) -> version) manifests in
  let satisfied = List.nth manifests (max_satisfying versions v_constraint) in
  ignore satisfied;
  print_string v_constraint
