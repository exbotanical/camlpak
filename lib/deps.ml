open Lock

type deps_stack_item = {
  name : string;
  version : string;
  dependencies : (string * string) list;
}

type deps_stack = deps_stack_item Stack.t
type top_level_item = { version : string; url : string }
type unsatisifed_dep = { name : string; parent : string; url : string }

let top_level : (string, top_level_item) Hashtbl.t = Hashtbl.create 10
let unsatisfied : unsatisifed_dep list ref = ref []

module Stack = struct
  include Stack

  let to_list s =
    let rec aux acc = function
      | s when is_empty s -> acc
      | s -> aux (pop s :: acc) s
    in
    aux [] s
end

(* TODO: *)
let max_satisfying (versions : string list) (v_constraint : string) =
  print_string v_constraint;
  List.length versions - 1

let satisfies (_ : string) (_ : string) = true

let get_transitive_conflict (name : string) (version : string)
    (stack : deps_stack) =
  match
    Seq.find_index
      (fun el ->
        let dep_version, _ =
          List.find (fun (dep, _) -> dep = name) el.dependencies
        in

        satisfies version dep_version)
      (Stack.to_seq stack)
  with
  | Some idx -> idx
  | None -> -1

let has_circular_dep (name : string) (version : string) (stack : deps_stack) =
  let seq : deps_stack_item Seq.t = Stack.to_seq stack in

  let is_circular (dep : deps_stack_item) =
    dep.name = name && satisfies dep.version version
  in

  match Seq.find is_circular seq with Some _ -> true | None -> false

let rec get_deps ?(stack : deps_stack = Stack.create ()) (name : string)
    (v_constraint : string) =
  let manifests =
    match Lock.get_dep name v_constraint with
    | Some dep -> dep
    | None ->
        Registry.get_manifest_from_registry name (module Registry.NpmRegistry)
  in

  let versions = List.map (fun (version, _) -> version) manifests in
  let satisfied_v, satisfied =
    List.nth manifests (max_satisfying versions v_constraint)
  in

  let notfound = ref false in

  let found_top_level =
    try Hashtbl.find top_level name
    with Not_found ->
      notfound := true;
      let item = { version = satisfied_v; url = satisfied.dist.tarball } in
      Hashtbl.add top_level name item;
      item
  in

  (*
    Ugh, this is where ocaml gets ugly...no early return?
    I wish it had something like Kotlin's scoped returns e.g. return@forEach
  *)
  let should_early_return = ref false in

  (if !notfound then
     if satisfies found_top_level.version v_constraint then
       let conflict_idx = get_transitive_conflict name satisfied_v stack in

       if conflict_idx > -1 then
         let as_list : deps_stack_item list = Stack.to_list stack in
         let extract_name (dep : deps_stack_item) = dep.name in

         let result =
           as_list |> List.map extract_name
           |> (fun lst ->
                let rec drop n lst =
                  if n = 0 then lst else drop (n - 1) (List.tl lst)
                in
                drop (conflict_idx - 2) lst)
           |> String.concat "/node_modules/"
         in

         unsatisfied :=
           { url = satisfied.dist.tarball; name; parent = result }
           :: !unsatisfied
       else should_early_return := true
     else
       let result = stack |> Stack.to_list |> List.rev |> List.hd in

       unsatisfied :=
         { url = satisfied.dist.tarball; name; parent = result.name }
         :: !unsatisfied);

  if !should_early_return then None
  else (
    Lock.upsert
      (name ^ "@" ^ v_constraint)
      {
        version = satisfied_v;
        url = satisfied.dist.tarball;
        shasum = satisfied.dist.shasum;
        dependencies = satisfied.dependencies;
      };

    if List.is_empty satisfied.dependencies then (
      let stack_dep =
        { name; version = satisfied_v; dependencies = satisfied.dependencies }
      in

      Stack.push stack_dep stack;

      List.iter
        (fun (dep, version) -> ignore (get_deps dep version ~stack))
        (List.filter
           (fun (dep, version) -> has_circular_dep dep version stack)
           satisfied.dependencies);

      ignore (Stack.pop stack));

    if v_constraint == "" then
      Some { name; version = "^" ^ satisfied_v; dependencies = [] }
    else None)

let get_all_deps (manifest : Types.manifest) =
  let all_deps =
    List.filter_map
      (fun (dep, version) -> get_deps dep version)
      manifest.dependencies
  in

  let update_virtual_manifest (dep : deps_stack_item) =
    ignore (List.append manifest.dependencies [ (dep.name, dep.version) ])
  in

  List.iter update_virtual_manifest all_deps;

  (top_level, unsatisfied)
