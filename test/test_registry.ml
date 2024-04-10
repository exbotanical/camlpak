open OUnit2

module type RetVal = sig
  val ret : string
end

module Ok : RetVal = struct
  let ret = Util.string_of_fixture "npm_fetched.json"
end

module NotOkInvalid : RetVal = struct
  let ret = "{}"
end

module MockRegistry (Arg : RetVal) : Camlpak.Registry.Registry = struct
  let get (_ : string) : string Lwt.t = Lwt.return Arg.ret
end

let registry_fetch_ok _ =
  let ret =
    Camlpak.Registry.get_manifest_from_registry "test"
      (module MockRegistry (Ok))
  in
  assert_equal 29 (List.length ret)

let registry_fetch_invalid _ =
  assert_raises (Camlpak.Json.ManifestError "No versions found") (fun _ ->
      Camlpak.Registry.get_manifest_from_registry "test"
        (module MockRegistry (NotOkInvalid)))

let tests =
  "test suite for registry"
  >::: [
         "fetches data from registry and deserializes" >:: registry_fetch_ok;
         "raises when registry data is invalid" >:: registry_fetch_invalid;
       ]

let _ = run_test_tt_main tests
