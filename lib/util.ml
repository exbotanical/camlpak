let get_env_var (var : string) (default : string) =
  try Sys.getenv var with Not_found -> default
