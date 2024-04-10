open OUnit2
open Camlpak.Lock
open Camlpak.Types

let lock_get_dep_present _ =
  let expected =
    {
      dependencies = [ ("x", "2.3.1") ];
      shasum = "xsm";
      url = "tt";
      version = "2.0.0";
    }
  in

  Hashtbl.add Camlpak.Lock.old_lock "test@1.0.0" expected;
  let actual = Camlpak.Lock.get_dep "test" "1.0.0" in

  assert_equal
    (Some
       [
         ( "1.0.0",
           {
             dependencies = expected.dependencies;
             dist = { tarball = expected.url; shasum = expected.shasum };
           } );
       ])
    actual

let lock_get_dep_not_present _ =
  let actual = Camlpak.Lock.get_dep "testx" "11.0.0" in

  assert_equal None actual

let tests =
  "test suite for lock"
  >::: [
         "gets dep from lockfile object when present" >:: lock_get_dep_present;
         "returns none when not present in lockfile object "
         >:: lock_get_dep_not_present;
       ]

let _ = run_test_tt_main tests
