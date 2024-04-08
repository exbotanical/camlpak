open OUnit2
open Camlpak.Types

let string_of_fixture fix =
  let cwd = Unix.getcwd () in
  let manifest_path = cwd ^ "/../../../test/fixtures/" ^ fix in
  let channel = open_in manifest_path in
  let file_content = In_channel.input_all channel in
  close_in channel;
  file_content

let test_manifest_of_file_ok _ =
  let cwd = Unix.getcwd () in
  let manifest_path = cwd ^ "/../../../test/fixtures/valid_manifest.json" in

  let parsed = Camlpak.Json.manifest_of_file manifest_path in

  let first_dep, first_dep_v = List.nth parsed.dependencies 0 in
  let second_dep, second_dep_v = List.nth parsed.dependencies 1 in
  let first_script_name, first_script = List.nth parsed.scripts 0 in
  let first_dir_k, first_dir_v = List.nth parsed.directories 0 in
  let second_dir_k, second_dir_v = List.nth parsed.directories 1 in

  assert_equal (List.length parsed.dependencies) 2;
  assert_equal first_dep "hello";
  assert_equal first_dep_v "1.0.0";
  assert_equal second_dep "goodbye";
  assert_equal second_dep_v "^2.3.1";

  assert_equal (List.length parsed.scripts) 1;
  assert_equal first_script_name "test";
  assert_equal first_script "echo \"Error: no test specified\" && exit 1";

  assert_equal (List.length parsed.directories) 2;
  assert_equal first_dir_k "libk";
  assert_equal first_dir_v "libv";
  assert_equal second_dir_k "testk";
  assert_equal second_dir_v "testv";

  assert_equal (List.length parsed.keywords) 4;
  assert_equal (List.nth parsed.keywords 0) "a";
  assert_equal (List.nth parsed.keywords 1) "b";
  assert_equal (List.nth parsed.keywords 2) "c";
  assert_equal (List.nth parsed.keywords 3) "d";

  assert_equal parsed.name "camlpak";
  assert_equal parsed.version "1.0.0";
  assert_equal parsed.description "some desc";
  assert_equal parsed.main "index.js";
  assert_equal parsed.author "the author";
  assert_equal parsed.license "ISC"

let test_manifest_of_file_missing_fields _ =
  let cwd = Unix.getcwd () in
  let manifest_path = cwd ^ "/../../../test/fixtures/invalid_manifest.json" in

  assert_raises
    (Camlpak.Json.ManifestError "Parsing error: Expected string, got null")
    (fun () -> Camlpak.Json.manifest_of_file manifest_path)

let test_manifest_of_file_bad_json _ =
  let cwd = Unix.getcwd () in
  let manifest_path = cwd ^ "/../../../test/fixtures/bad_json.json" in

  assert_raises
    (Camlpak.Json.ManifestError
       "Invalid JSON: Line 1, bytes 1-2:\nUnexpected end of input") (fun () ->
      Camlpak.Json.manifest_of_file manifest_path)

let test_parse_manifest_list _ =
  let get_first (x, _) = x in
  let get_snd (_, y) = y in

  let file_content = string_of_fixture "npm_fetched.json" in

  let parsed = Camlpak.Json.dep_entry_list_of_json file_content in

  assert_equal 29 (List.length parsed);
  assert_equal "0.0.2" (get_first (List.nth parsed 0));
  assert_equal "0.15.0" (get_first (List.nth parsed 28));
  assert_equal
    {
      dist =
        {
          shasum = "7f3dbcb32341bd6b3aaad2f415da14a4aaffe461";
          tarball =
            "https://registry.npmjs.org/@magister_zito/eslint-config/-/eslint-config-0.15.0.tgz";
        };
      dependencies =
        [
          ("@magister_zito/eslint-config-react", "^0.15.0");
          ("@magister_zito/eslint-config-vue", "^0.15.0");
          ("@typescript-eslint/eslint-plugin", "^5.48.1");
          ("@typescript-eslint/parser", "^5.48.1");
          ("eslint-config-standard", "^17.0.0");
          ("eslint-plugin-eslint-comments", "^3.2.0");
          ("eslint-plugin-html", "^7.1.0");
          ("eslint-plugin-import", "^2.26.0");
          ("eslint-plugin-jsonc", "^2.6.0");
          ("eslint-plugin-node", "^11.1.0");
          ("eslint-plugin-promise", "6.1.1");
          ("eslint-plugin-vue", "9.8.0");
          ("eslint-plugin-yml", "^1.4.0");
          ("jsonc-eslint-parser", "^2.1.0");
          ("yaml-eslint-parser", "^1.1.0");
        ];
    }
    (get_snd (List.nth parsed 28))

let test_parse_manifest_list_invalid _ =
  let invalid_list = "{\"whatever\":\"ok\"}" in

  assert_raises (Camlpak.Json.ManifestError "No versions found") (fun () ->
      Camlpak.Json.dep_entry_list_of_json invalid_list)

let tests =
  "test suite for json"
  >::: [
         "ok - manifest" >:: test_manifest_of_file_ok;
         "not ok - missing required fields"
         >:: test_manifest_of_file_missing_fields;
         "not ok - bad json" >:: test_manifest_of_file_bad_json;
         "ok - manifest list" >:: test_parse_manifest_list;
         "not ok - manifest list missing versions"
         >:: test_parse_manifest_list_invalid;
       ]

let _ = run_test_tt_main tests
