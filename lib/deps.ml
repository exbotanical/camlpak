type deps_stack_item = {
  name : string;
  version : string;
  dependencies : string * string;
}

type deps_stack = deps_stack_item list

open Lwt
open Cohttp
open Cohttp_lwt_unix

let fetch_data (url : string) : string Lwt.t =
  Client.get (Uri.of_string url) >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body

let () =
  let url = "http://example.com/data" in
  Lwt_main.run (fetch_data url >>= fun response -> Lwt_io.printl response)

let get_deps (name : string) (v_constraint : string) (stack : deps_stack) =
  let from_lock = Lock.get_item name v_constraint in

  match from_lock with
  | Some dep -> print_string dep.name
  | None -> print_string "nope"

(* async function collectDeps(
  name: string,
  constraint: string,
  stack: DependencyStack = [],
) {
  // Retrieve a single manifest by name from the lock.
  const fromLock = lock.getItem(name, constraint)

  /*
   * Fetch the manifest information.
   * If that manifest is not existed in the lock,
   * fetch it from network.
   */
  const manifest = fromLock || (await resolve(name))

  // Add currently resolving module to CLI
  log.logResolving(name)

  /*
   * Use the latest version of a package
   * while it will conform the semantic version.
   * However, if no semantic version is specified,
   * use the latest version.
   */
  const versions = Object.keys(manifest)
  const matched = constraint
    ? semver.maxSatisfying(versions, constraint)
    : versions[versions.length - 1] // The last one is the latest.
  if (!matched) {
    throw new Error('Cannot resolve suitable package.')
  }

  const matchedManifest = manifest[matched]!

  if (!topLevel[name]) {
    /*
     * If this package is not existed in the `topLevel` map,
     * just put it.
     */
    topLevel[name] = { url: matchedManifest.dist.tarball, version: matched }
  } else if (semver.satisfies(topLevel[name]!.version, constraint)) {
    const conflictIndex = checkStackDependencies(name, matched, stack)
    if (conflictIndex === -1) {
      /*
       * Remember to return this function to skip the dependencies checking.
       * This may avoid dependencies circulation.
       */
      return
    }
    /*
     * Because of the module resolution algorithm of Node.js,
     * there may be some conflicts in the dependencies of dependency.
     * How to check it? See the `checkStackDependencies` function below.
     * ----------------------------
     * We just need information of the previous **two** dependencies
     * of the dependency which has conflicts.
     * :(  Not sure if it's right.
     */
    unsatisfied.push({
      name,
      parent: stack
        .map(({ name }) => name) // eslint-disable-line no-shadow
        .slice(conflictIndex - 2)
        .join('/node_modules/'),
      url: matchedManifest.dist.tarball,
    })
  } else {
    /*
     * Yep, the package is already existed in that map,
     * but it has conflicts because of the semantic version.
     * So we should add a record.
     */
    unsatisfied.push({
      name,
      parent: stack.at(-1)!.name,
      url: matchedManifest.dist.tarball,
    })
  }

  // Don't forget to collect the dependencies of our dependencies.
  const dependencies = matchedManifest.dependencies ?? {}

  // Save the manifest to the new lock.
  lock.updateOrCreate(`${name}@${constraint}`, {
    version: matched,
    url: matchedManifest.dist.tarball,
    shasum: matchedManifest.dist.shasum,
    dependencies,
  })

  /*
   * Collect the dependencies of dependency,
   * so it's time to be deeper.
   */
  if (dependencies) {
    stack.push({
      name,
      version: matched,
      dependencies,
    })
    await Promise.all(
      Object.entries(dependencies)
        // The filter below is to prevent dependency circulation
        .filter(([dep, range]) => !hasCirculation(dep, range, stack))
        .map(([dep, range]) => collectDeps(dep, range, stack.slice()))
    )
    stack.pop()
  }

  /*
   * Return the semantic version range to
   * add missing semantic version range in `package.json`.
   */
  if (!constraint) {
    return { name, version: `^${matched}` }
  }
}

/**
 * This function is to check if there are conflicts in the
 * dependencies of dependency, not the top level dependencies.
 */
function checkStackDependencies(
  name: string,
  version: string,
  stack: DependencyStack,
) {
  return stack.findIndex(({ dependencies }) => {
    const semverRange = dependencies[name]
    /*
     * If this package is not as a dependency of another package,
     * this is safe and we just return `true`.
     */
    if (!semverRange) {
      return true
    }

    // Semantic version checking.
    return semver.satisfies(version, semverRange)
  })
} *)
