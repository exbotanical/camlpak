open OUnit2

let test_manifest_of_file_ok _ =
  let cwd = Unix.getcwd () in
  let manifest_path = cwd ^ "/../../../test/fixtures/valid_manifest.json" in

  let parsed = Camlpak.Manifest.manifest_of_file manifest_path in

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

let tests =
  "test suite for manifest_of_file"
  >::: [
         "ok" >:: test_manifest_of_file_ok;
         (* "not ok - missing required fields" >:: test_manifest_of_file_ok;
            "not ok - bad json" >:: test_manifest_of_file_ok; *)
       ]

let _ = run_test_tt_main tests
