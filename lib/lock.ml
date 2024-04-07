type lock_entry = {
  version : string;
  url : string;
  shasum : string;
  dependencies : (string * string) list;
}

let new_lock : (string, lock_entry) Hashtbl.t = Hashtbl.create 10
let old_lock : (string, lock_entry) Hashtbl.t = Hashtbl.create 10

let get_item (name : string) (v_constraint : string) =
  let key = name ^ "@" ^ v_constraint in

  let value : lock_entry option = Hashtbl.find_opt old_lock key in

  match value with
  | None -> None
  | Some lock_entry ->
      Some
        (let ht : Types.version_manifest = Hashtbl.create 1 in
         Hashtbl.add ht lock_entry.version
           {
             dist = { shasum = lock_entry.shasum; tarball = lock_entry.url };
             dependencies = lock_entry.dependencies;
           };

         ht)
