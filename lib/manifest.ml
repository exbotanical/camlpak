module YojsonUtil = Yojson.Basic.Util

type manifest = {
  name : string;
  version : string;
  description : string;
  main : string;
  author : string;
  license : string;
  keywords : string list;
  directories : (string * string) list;
  scripts : (string * string) list;
  dependencies : (string * string) list;
}

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

let parse_manifest raw_json =
  try manifest_of_json raw_json
  with YojsonUtil.Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let manifest_of_file fp = fp |> Yojson.Basic.from_file |> parse_manifest
