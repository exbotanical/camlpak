type distribution = { shasum : string; tarball : string }

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

type dep_entry_todo = {
  dist : distribution;
  dependencies : (string * string) list;
}
