open Types
module YojsonUtil = Yojson.Basic.Util

exception ManifestError of string

let extract_associative_list (raw_json : Yojson.Basic.t) (key : string) :
    (string * string) list =
  match raw_json |> YojsonUtil.member key with
  | `Assoc dep -> List.map (fun (k, v) -> (k, YojsonUtil.to_string v)) dep
  | _ -> []

let manifest_of_json raw_json =
  {
    name = raw_json |> YojsonUtil.member "name" |> YojsonUtil.to_string;
    version = raw_json |> YojsonUtil.member "version" |> YojsonUtil.to_string;
    description =
      raw_json |> YojsonUtil.member "description" |> YojsonUtil.to_string;
    (* TODO: these should not be required *)
    main = raw_json |> YojsonUtil.member "main" |> YojsonUtil.to_string;
    author = raw_json |> YojsonUtil.member "author" |> YojsonUtil.to_string;
    license = raw_json |> YojsonUtil.member "license" |> YojsonUtil.to_string;
    keywords =
      (match raw_json |> YojsonUtil.member "keywords" with
      | `List lst -> List.map YojsonUtil.to_string lst
      | _ -> []);
    directories = extract_associative_list raw_json "directories";
    scripts = extract_associative_list raw_json "scripts";
    dependencies = extract_associative_list raw_json "dependencies";
  }

let dep_entry_todo_of_json raw_json =
  (* { dist:  { tarball: "", shasum: "" } } *)
  let dist = raw_json |> YojsonUtil.member "dist" in

  {
    dist =
      {
        tarball = dist |> YojsonUtil.member "tarball" |> YojsonUtil.to_string;
        shasum = dist |> YojsonUtil.member "shasum" |> YojsonUtil.to_string;
      };
    dependencies = extract_associative_list raw_json "dependencies";
  }

let parse_manifest raw_json =
  try manifest_of_json raw_json
  with YojsonUtil.Type_error (s, _) ->
    raise (ManifestError ("Parsing error: " ^ s))

let parse_dep_entry_todo raw_json =
  try dep_entry_todo_of_json raw_json
  with YojsonUtil.Type_error (s, _) ->
    raise (ManifestError ("Parsing error: " ^ s))

let manifest_of_file fp =
  try fp |> Yojson.Basic.from_file |> parse_manifest
  with Yojson.Json_error s -> raise (ManifestError ("Invalid JSON: " ^ s))

let dep_entry_todo_of_string str =
  str |> Yojson.Basic.from_string |> parse_dep_entry_todo

let extract_associative_list_of_manifests (raw_json : Yojson.Basic.t)
    (key : string) =
  match raw_json |> YojsonUtil.member key with
  | `Assoc dep -> List.map (fun (k, v) -> (k, parse_dep_entry_todo v)) dep
  | _ -> []

let dep_entry_list_of_json raw_json =
  let raw_t = raw_json |> Yojson.Basic.from_string in

  match extract_associative_list_of_manifests raw_t "versions" with
  | [] -> raise (ManifestError "No versions found")
  | d -> d
