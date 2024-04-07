type distribution = { shasum : string; tarball : string }

type version_manifest_entry = {
  dist : distribution;
  dependencies : (string * string) list;
}

type version_manifest = (string, version_manifest_entry) Hashtbl.t
