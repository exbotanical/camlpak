let get_fixture_abs_path name =
  let cwd = Unix.getcwd () in
  cwd ^ "/../../../test/fixtures/" ^ name

let string_of_fixture fix =
  let manifest_path = get_fixture_abs_path fix in
  let channel = open_in manifest_path in
  let file_content = In_channel.input_all channel in
  close_in channel;
  file_content
