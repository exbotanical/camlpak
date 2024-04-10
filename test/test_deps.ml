open Camlpak.Deps

let () =
  let manifest =
    Camlpak.Json.manifest_of_file (Util.get_fixture_abs_path "real_deps.json")
  in

  let top_level, unsatisfied = Camlpak.Deps.get_all_deps manifest in

  List.iter (fun dep -> print_endline dep.name) !unsatisfied;

  let print_ht_entry k (v : top_level_item) =
    print_endline k;
    print_endline v.url
  in

  Hashtbl.iter print_ht_entry top_level
