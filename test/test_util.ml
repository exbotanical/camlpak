open OUnit2

(* let get_env_var_ok _ =
   let key = "S_S_S_S_S_S_S_S_" in
   let value = "t" in

   Unix.putenv key value;
   let ret = Camlpak.Util.get_env_var key "wat" in
   assert_equal value ret *)

let get_env_var_ok_default _ =
  let value = "t" in

  let ret = Camlpak.Util.get_env_var "xS_S_S_S_S_S_S_S_" value in
  assert_equal value ret

let tests =
  "test suite for util"
  >::: [
         (* "get_env_var returns environment variable when present"
            >:: get_env_var_ok; *)
         "get_env_var returns default value when environment variable not \
          present" >:: get_env_var_ok_default;
       ]

let _ = run_test_tt_main tests
