type lock_entry = {
  version : string;
  url : string;
  shasum : string;
  dependencies : (string * string) list;
}

let new_lock : (string, lock_entry) Hashtbl.t = Hashtbl.create 10
let old_lock : (string, lock_entry) Hashtbl.t = Hashtbl.create 10

let get_dep (name : string) (v_constraint : string) =
  let key = name ^ "@" ^ v_constraint in
  let value = Hashtbl.find_opt old_lock key in

  match value with
  | Some v ->
      let entry : Types.dep_entry_todo =
        {
          dependencies = v.dependencies;
          dist = { shasum = v.shasum; tarball = v.url };
        }
      in

      Some [ (v_constraint, entry) ]
  | None -> None

let upsert (key : string) (value : lock_entry) = Hashtbl.add new_lock key value
