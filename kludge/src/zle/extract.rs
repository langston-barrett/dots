use std::{
    collections::{HashMap, HashSet},
    path::Path,
    process::Command,
};

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub(super) struct Cmd {
    pub(super) short: String,
    pub(super) long: String,
    pub(super) flags: HashMap<String, String>,
    pub(super) subs: Vec<Cmd>,
}

// TODO recursive
#[derive(Debug, Default, serde::Deserialize)]
pub(crate) struct ConfigFile {
    extra_subs: Vec<String>,
    flags: HashMap<String, String>,
    subs: HashMap<String, String>,
}

impl ConfigFile {
    pub(super) fn from_file<P: AsRef<Path>>(p: P) -> Self {
        toml::from_str::<ConfigFile>(&std::fs::read_to_string(p).unwrap()).unwrap()
    }
}

fn unique_prefixes(strings: &[String]) -> HashMap<String, String> {
    let mut result = HashMap::with_capacity(strings.len());
    for string in strings.iter() {
        let mut pfx = String::new();
        for c in string.chars() {
            pfx.push(c);
            let is_unique = strings.iter().filter(|s| s.starts_with(&pfx)).count() == 1;
            if is_unique {
                break;
            }
        }
        result.insert(string.clone(), pfx);
    }
    result
}

fn deconflict_flags(conf: &ConfigFile, flags: &[String]) -> HashMap<String, String> {
    let mut result = HashMap::with_capacity(conf.flags.len());
    let mut rest = Vec::with_capacity(flags.len() - conf.flags.len());
    for string in flags.iter() {
        if let Some(short) = conf.flags.get(string) {
            result.insert(string.clone(), short.clone());
        } else {
            rest.push(string.clone());
        }
    }
    let shorts = conf.flags.values().cloned().collect::<Vec<String>>();
    rest.extend(shorts.iter().cloned());
    result.extend(unique_prefixes(&rest));
    for short in shorts {
        result.remove(&short);
    }
    result
}

fn deconflict_subs(conf: &ConfigFile, subs: &[String]) -> HashMap<String, String> {
    let mut result = HashMap::with_capacity(conf.subs.len());
    let mut rest = Vec::with_capacity(subs.len() - conf.subs.len());
    for string in subs.iter() {
        if let Some(short) = conf.subs.get(string) {
            result.insert(string.clone(), short.clone());
        } else {
            rest.push(string.clone());
        }
    }
    let shorts = conf.subs.values().cloned().collect::<Vec<String>>();
    rest.extend(shorts.iter().cloned());
    result.extend(unique_prefixes(&rest));
    for short in shorts {
        result.remove(&short);
    }
    result
}

fn extract_sub(words: &[&str]) -> Option<String> {
    if words.len() < 2 {
        return None;
    }
    let first = &words[0];
    debug_assert!(!first.is_empty());
    let first_char = first.chars().next().unwrap();

    if first_char.is_lowercase()
            // The description comes immediately after the subcommand, or, in the
            // case of cargo aliases, just after the alias. The description
            // usually starts with a capital.
            && (words[1].chars().next().unwrap().is_uppercase()
                || (words.len() > 2 && words[2].chars().next().unwrap().is_uppercase()))
            && first.is_ascii()
    {
        let long = first.chars().filter(|c| *c != ',').collect::<String>();
        return Some(long);
    }
    None
}

#[derive(Debug)]
struct Opt {
    #[allow(dead_code)]
    short: Option<String>,
    long: String,
}

fn extract_opt(mut words: &[&str]) -> Option<Opt> {
    const LONG: &[char] = &['-', '-'];
    if words.is_empty() {
        return None;
    }
    let mut first = &words[0];
    let short = if first.starts_with('-') && !first.starts_with(LONG) {
        words = &words[1..];
        let mut chars = first.chars();
        chars.next();
        Some(chars.collect::<String>())
    } else {
        None
    };

    while !words.is_empty() {
        first = &words[0];
        words = &words[1..];
        if !first.starts_with(LONG) {
            continue;
        }
        let mut opt = &first[LONG.len()..];
        if let Some(idx) = opt.find('=') {
            opt = &opt[..idx];
        }
        if let Some(idx) = opt.find('[') {
            opt = &opt[..idx];
        }
        let long = opt
            .chars()
            .filter(|c| c.is_alphabetic())
            .collect::<String>();
        if long.len() <= 2 {
            continue;
        }
        return Some(Opt { short, long });
    }
    None
}

#[allow(dead_code)]
fn extract_text(conf: &ConfigFile, text: String) -> (HashMap<String, String>, Vec<Cmd>) {
    let mut opts = Vec::new();
    let mut sub_names = Vec::new();
    for mut line in text.lines() {
        if !line.starts_with([' ', ' ']) {
            continue;
        }
        line = line.trim_start();
        let words = line.split_whitespace().collect::<Vec<_>>();

        if let Some(long) = extract_sub(words.as_slice()) {
            sub_names.push(long);
        }

        if !line.contains(['-', '-']) {
            continue;
        }
        if let Some(opt) = extract_opt(words.as_slice()) {
            opts.push(opt);
        }
    }

    let existing: HashSet<String> = HashSet::from_iter(sub_names.iter().cloned());
    sub_names.extend(
        conf.extra_subs
            .iter()
            .filter(|n| !existing.contains(*n))
            .cloned(),
    );
    let mut subs = Vec::with_capacity(sub_names.len());
    let sub_pairs = deconflict_subs(conf, sub_names.as_slice());
    for (long, short) in sub_pairs.into_iter() {
        subs.push(Cmd {
            short,
            long,
            flags: HashMap::new(),
            subs: Vec::new(),
        })
    }

    let opts = opts.into_iter().map(|o| o.long).collect::<Vec<_>>();
    let flag_pairs = deconflict_flags(conf, opts.as_slice());
    let mut flags = HashMap::<String, String>::new();
    for (long, short) in flag_pairs.into_iter() {
        if let Some(v) = flags.get(&short) {
            assert_eq!(*v, long);
        }
        flags.insert(short, long);
    }
    (flags, subs)
}

fn help(cmd: &str) -> Option<String> {
    let output = Command::new(cmd).arg("--help").output().unwrap();
    String::from_utf8(output.stdout).ok()
}

// TODO recur
pub(super) fn extract(conf: ConfigFile, long: String) -> Cmd {
    let h = help(&long).unwrap();
    let (flags, subs) = extract_text(&conf, h);
    Cmd {
        short: String::from(long.chars().next().unwrap()),
        long,
        flags,
        subs,
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::{extract_text, ConfigFile};

    const CABAL_HELP: &str = r#"
Command line interface to the Haskell Cabal infrastructure.

See http://www.haskell.org/cabal/ for more information.

Usage: cabal [GLOBAL FLAGS] [COMMAND [FLAGS]]

Commands:
 [global]
  user-config            Display and update the user's global cabal configuration.
  help                   Help about commands.

 [package database]
  update                 Updates list of known packages.
  list                   List packages matching a search string.
  info                   Display detailed information about a particular package.

 [initialization and download]
  init                   Create a new cabal package.
  fetch                  Downloads packages for later installation.
  get                    Download/Extract a package's source code (repository).

 [project configuration]
  configure              Add extra project configuration.
  freeze                 Freeze dependencies.
  gen-bounds             Generate dependency bounds.
  outdated               Check for outdated dependencies.

 [project building and installing]
  build                  Compile targets within the project.
  install                Install packages.
  haddock                Build Haddock documentation.
  haddock-project        Generate Haddocks HTML documentation for the cabal project.
  clean                  Clean the package store and remove temporary files.

 [running and testing]
  list-bin               List the path to a single executable.
  repl                   Open an interactive session for the given component.
  run                    Run an executable.
  bench                  Run benchmarks.
  test                   Run test-suites.
  exec                   Give a command access to the store.

 [sanity checks and shipping]
  check                  Check the package for common mistakes.
  sdist                  Generate a source distribution file (.tar.gz).
  upload                 Uploads source packages or documentation to Hackage.
  report                 Upload build reports to a remote server.

 [deprecated]
  unpack                 Deprecated alias for 'get'.
  hscolour               Generate HsColour colourised code, in HTML format.

 [new-style projects (forwards-compatible aliases)]
  v2-build               Compile targets within the project.
  v2-configure           Add extra project configuration.
  v2-repl                Open an interactive session for the given component.
  v2-run                 Run an executable.
  v2-test                Run test-suites.
  v2-bench               Run benchmarks.
  v2-freeze              Freeze dependencies.
  v2-haddock             Build Haddock documentation.
  v2-exec                Give a command access to the store.
  v2-update              Updates list of known packages.
  v2-install             Install packages.
  v2-clean               Clean the package store and remove temporary files.
  v2-sdist               Generate a source distribution file (.tar.gz).

 [legacy command aliases]
  v1-build               Compile all/specific components.
  v1-configure           Prepare to build the package.
  v1-repl                Open an interpreter session for the given component.
  v1-run                 Builds and runs an executable.
  v1-test                Run all/specific tests in the test suite.
  v1-bench               Run all/specific benchmarks.
  v1-freeze              Freeze dependencies.
  v1-haddock             Generate Haddock HTML documentation.
  v1-install             Install packages.
  v1-clean               Clean up after a build.
  v1-copy                Copy the files of all/specific components to install locations.
  v1-register            Register this package with the compiler.
  v1-reconfigure         Reconfigure the package if necessary.

 [other]
  haddock-project        Generate Haddocks HTML documentation for the cabal project.
  new-haddock-project    Generate Haddocks HTML documentation for the cabal project.
  v2-haddock-project     Generate Haddocks HTML documentation for the cabal project.

For more information about a command use:
   cabal COMMAND --help
or cabal help COMMAND

To install Cabal packages from hackage use:
  cabal install foo [--dry-run]

Occasionally you need to update the list of available packages:
  cabal update

Global flags:
 -h, --help                     Show this help text
 -V, --version                  Print version information
 --numeric-version              Print just the version number
 --config-file=FILE             Set an alternate location for the config file
 --ignore-expiry                Ignore expiry dates on signed metadata (use
                                only in exceptional circumstances)
 --http-transport=HttpTransport
                                Set a transport for http(s) requests. Accepts
                                'curl', 'wget', 'powershell', and
                                'plain-http'. (default: 'curl')
 --nix[=(True or False)]        Nix integration: run commands through
                                nix-shell if a 'shell.nix' file exists
                                (default is False)
 --enable-nix                   Enable Nix integration: run commands through
                                nix-shell if a 'shell.nix' file exists
 --disable-nix                  Disable Nix integration
 --store-dir=DIR                The location of the build store
 --active-repositories=REPOS    The active package repositories (set to
                                ':none' to disable all repositories)"#;

    const CARGO_HELP: &str = r#"
Rust's package manager

Usage: cargo [+toolchain] [OPTIONS] [COMMAND]

Options:
  -V, --version             Print version info and exit
      --list                List installed commands
      --explain <CODE>      Run `rustc --explain CODE`
  -v, --verbose...          Use verbose output (-vv very verbose/build.rs output)
  -q, --quiet               Do not print cargo log messages
      --color <WHEN>        Coloring: auto, always, never
  -C <DIRECTORY>            Change to DIRECTORY before doing anything (nightly-only)
      --frozen              Require Cargo.lock and cache are up to date
      --locked              Require Cargo.lock is up to date
      --offline             Run without accessing the network
      --config <KEY=VALUE>  Override a configuration value
  -Z <FLAG>                 Unstable (nightly-only) flags to Cargo, see 'cargo -Z help' for details
  -h, --help                Print help

Some common cargo commands are (see all commands with --list):
    build, b    Compile the current package
    check, c    Analyze the current package and report errors, but don't build object files
    clean       Remove the target directory
    doc, d      Build this package's and its dependencies' documentation
    new         Create a new cargo package
    init        Create a new cargo package in an existing directory
    add         Add dependencies to a manifest file
    remove      Remove dependencies from a manifest file
    run, r      Run a binary or example of the local package
    test, t     Run the tests
    bench       Run the benchmarks
    update      Update dependencies listed in Cargo.lock
    search      Search registry for crates
    publish     Package and upload this package to the registry
    install     Install a Rust binary. Default location is $HOME/.cargo/bin
    uninstall   Uninstall a Rust binary

See 'cargo help <command>' for more information on a specific command."#;

    const DOCKER_HELP: &str = r#"
Usage:  docker [OPTIONS] COMMAND

A self-sufficient runtime for containers

Options:
      --config string      Location of client config files (default "/home/langston/.docker")
  -c, --context string     Name of the context to use to connect to the daemon (overrides
                           DOCKER_HOST env var and default context set with "docker context use")
  -D, --debug              Enable debug mode
  -H, --host list          Daemon socket(s) to connect to
  -l, --log-level string   Set the logging level ("debug"|"info"|"warn"|"error"|"fatal") (default
                           "info")
      --tls                Use TLS; implied by --tlsverify
      --tlscacert string   Trust certs signed only by this CA (default
                           "/home/langston/.docker/ca.pem")
      --tlscert string     Path to TLS certificate file (default "/home/langston/.docker/cert.pem")
      --tlskey string      Path to TLS key file (default "/home/langston/.docker/key.pem")
      --tlsverify          Use TLS and verify the remote
  -v, --version            Print version information and quit

Management Commands:
  builder     Manage builds
  buildx*     Docker Buildx (Docker Inc., 0.0.0+unknown)
  compose*    Docker Compose (Docker Inc., 2.5.1)
  config      Manage Docker configs
  container   Manage containers
  context     Manage contexts
  image       Manage images
  manifest    Manage Docker image manifests and manifest lists
  network     Manage networks
  node        Manage Swarm nodes
  plugin      Manage plugins
  secret      Manage Docker secrets
  service     Manage services
  stack       Manage Docker stacks
  swarm       Manage Swarm
  system      Manage Docker
  trust       Manage trust on Docker images
  volume      Manage volumes

Commands:
  attach      Attach local standard input, output, and error streams to a running container
  build       Build an image from a Dockerfile
  commit      Create a new image from a container's changes
  cp          Copy files/folders between a container and the local filesystem
  create      Create a new container
  diff        Inspect changes to files or directories on a container's filesystem
  events      Get real time events from the server
  exec        Run a command in a running container
  export      Export a container's filesystem as a tar archive
  history     Show the history of an image
  images      List images
  import      Import the contents from a tarball to create a filesystem image
  info        Display system-wide information
  inspect     Return low-level information on Docker objects
  kill        Kill one or more running containers
  load        Load an image from a tar archive or STDIN
  login       Log in to a Docker registry
  logout      Log out from a Docker registry
  logs        Fetch the logs of a container
  pause       Pause all processes within one or more containers
  port        List port mappings or a specific mapping for the container
  ps          List containers
  pull        Pull an image or a repository from a registry
  push        Push an image or a repository to a registry
  rename      Rename a container
  restart     Restart one or more containers
  rm          Remove one or more containers
  rmi         Remove one or more images
  run         Run a command in a new container
  save        Save one or more images to a tar archive (streamed to STDOUT by default)
  search      Search the Docker Hub for images
  start       Start one or more stopped containers
  stats       Display a live stream of container(s) resource usage statistics
  stop        Stop one or more running containers
  tag         Create a tag TARGET_IMAGE that refers to SOURCE_IMAGE
  top         Display the running processes of a container
  unpause     Unpause all processes within one or more containers
  update      Update configuration of one or more containers
  version     Show the Docker version information
  wait        Block until one or more containers stop, then print their exit codes

Run 'docker COMMAND --help' for more information on a command."#;

    const GIT_HELP: &str = "
usage: git [-v | --version] [-h | --help] [-C <path>] [-c <name>=<value>]
           [--exec-path[=<path>]] [--html-path] [--man-path] [--info-path]
           [-p | --paginate | -P | --no-pager] [--no-replace-objects] [--bare]
           [--git-dir=<path>] [--work-tree=<path>] [--namespace=<name>]
           [--super-prefix=<path>] [--config-env=<name>=<envvar>]
           <command> [<args>]

These are common Git commands used in various situations:

start a working area (see also: git help tutorial)
   clone     Clone a repository into a new directory
   init      Create an empty Git repository or reinitialize an existing one

work on the current change (see also: git help everyday)
   add       Add file contents to the index
   mv        Move or rename a file, a directory, or a symlink
   restore   Restore working tree files
   rm        Remove files from the working tree and from the index

examine the history and state (see also: git help revisions)
   bisect    Use binary search to find the commit that introduced a bug
   diff      Show changes between commits, commit and working tree, etc
   grep      Print lines matching a pattern
   log       Show commit logs
   show      Show various types of objects
   status    Show the working tree status

grow, mark and tweak your common history
   branch    List, create, or delete branches
   commit    Record changes to the repository
   merge     Join two or more development histories together
   rebase    Reapply commits on top of another base tip
   reset     Reset current HEAD to the specified state
   switch    Switch branches
   tag       Create, list, delete or verify a tag object signed with GPG

collaborate (see also: git help workflows)
   fetch     Download objects and refs from another repository
   pull      Fetch from and integrate with another repository or a local branch
   push      Update remote refs along with associated objects

'git help -a' and 'git help -g' list available subcommands and some
concept guides. See 'git help <command>' or 'git help <concept>'
to read about a specific subcommand or concept.
See 'git help git' for an overview of the system.";

    #[test]
    fn extract_cabal() {
        let (flags, subs) = extract_text(&ConfigFile::default(), String::from(CABAL_HELP));
        let mut subs = subs
            .iter()
            .map(|s| (s.short.as_str(), s.long.as_str()))
            .collect::<Vec<_>>();
        subs.sort();
        let mut flags = flags
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect::<Vec<_>>();
        flags.sort();
        assert_eq!(
            flags,
            [
                ("a", "activerepositories"),
                ("c", "configfile"),
                ("d", "disablenix"),
                ("e", "enablenix"),
                ("help", "help"),
                ("ht", "httptransport"),
                ("i", "ignoreexpiry"),
                ("ni", "nix"),
                ("nu", "numericversion"),
                ("s", "storedir"),
                ("v", "version")
            ]
        );
        assert_eq!(
            subs.as_slice(),
            [
                ("be", "bench"),
                ("bu", "build"),
                ("ca", "cabal"),
                ("ch", "check"),
                ("cl", "clean"),
                ("co", "configure"),
                ("e", "exec"),
                ("fe", "fetch"),
                ("fr", "freeze"),
                ("gen", "gen-bounds"),
                ("get", "get"),
                ("haddock", "haddock"),
                ("haddock-project", "haddock-project"),
                ("he", "help"),
                ("hs", "hscolour"),
                ("inf", "info"),
                ("ini", "init"),
                ("ins", "install"),
                ("list", "list"),
                ("list-", "list-bin"),
                ("n", "new-haddock-project"),
                ("o", "outdated"),
                ("repl", "repl"),
                ("repo", "report"),
                ("ru", "run"),
                ("s", "sdist"),
                ("t", "test"),
                ("un", "unpack"),
                ("upd", "update"),
                ("upl", "upload"),
                ("us", "user-config"),
                ("v1-be", "v1-bench"),
                ("v1-bu", "v1-build"),
                ("v1-cl", "v1-clean"),
                ("v1-con", "v1-configure"),
                ("v1-cop", "v1-copy"),
                ("v1-f", "v1-freeze"),
                ("v1-h", "v1-haddock"),
                ("v1-i", "v1-install"),
                ("v1-rec", "v1-reconfigure"),
                ("v1-reg", "v1-register"),
                ("v1-rep", "v1-repl"),
                ("v1-ru", "v1-run"),
                ("v1-t", "v1-test"),
                ("v2-be", "v2-bench"),
                ("v2-bu", "v2-build"),
                ("v2-cl", "v2-clean"),
                ("v2-co", "v2-configure"),
                ("v2-e", "v2-exec"),
                ("v2-f", "v2-freeze"),
                ("v2-haddock", "v2-haddock"),
                ("v2-haddock-", "v2-haddock-project"),
                ("v2-i", "v2-install"),
                ("v2-re", "v2-repl"),
                ("v2-ru", "v2-run"),
                ("v2-s", "v2-sdist"),
                ("v2-t", "v2-test"),
                ("v2-u", "v2-update")
            ]
        );
    }

    #[test]
    fn extract_cargo() {
        let (flags, subs) = extract_text(&ConfigFile::default(), String::from(CARGO_HELP));
        let mut subs = subs
            .iter()
            .map(|s| (s.short.as_str(), s.long.as_str()))
            .collect::<Vec<_>>();
        subs.sort();
        let mut flags = flags
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect::<Vec<_>>();
        flags.sort();
        assert_eq!(
            flags,
            [
                ("col", "color"),
                ("con", "config"),
                ("e", "explain"),
                ("f", "frozen"),
                ("h", "help"),
                ("li", "list"),
                ("lo", "locked"),
                ("o", "offline"),
                ("q", "quiet"),
                ("verb", "verbose"),
                ("vers", "version")
            ]
        );
        assert_eq!(
            subs.as_slice(),
            [
                ("a", "add"),
                ("be", "bench"),
                ("bu", "build"),
                ("ch", "check"),
                ("cl", "clean"),
                ("d", "doc"),
                ("ini", "init"),
                ("ins", "install"),
                ("n", "new"),
                ("p", "publish"),
                ("re", "remove"),
                ("ru", "run"),
                ("s", "search"),
                ("t", "test"),
                ("un", "uninstall"),
                ("up", "update")
            ]
        );
    }

    #[test]
    fn extract_docker() {
        let (flags, subs) = extract_text(&ConfigFile::default(), String::from(DOCKER_HELP));
        let mut subs = subs
            .iter()
            .map(|s| (s.short.as_str(), s.long.as_str()))
            .collect::<Vec<_>>();
        subs.sort();
        let mut flags = flags
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect::<Vec<_>>();
        flags.sort();
        assert_eq!(
            flags,
            [
                ("conf", "config"),
                ("cont", "context"),
                ("d", "debug"),
                ("h", "host"),
                ("l", "loglevel"),
                ("tls", "tls"),
                ("tlsca", "tlscacert"),
                ("tlsce", "tlscert"),
                ("tlsk", "tlskey"),
                ("tlsv", "tlsverify"),
                ("v", "version")
            ]
        );
        assert_eq!(
            subs.as_slice(),
            [
                ("a", "attach"),
                ("build", "build"),
                ("builde", "builder"),
                ("buildx", "buildx*"),
                ("comm", "commit"),
                ("comp", "compose*"),
                ("conf", "config"),
                ("conta", "container"),
                ("conte", "context"),
                ("cp", "cp"),
                ("cr", "create"),
                ("d", "diff"),
                ("ev", "events"),
                ("exe", "exec"),
                ("exp", "export"),
                ("h", "history"),
                ("image", "image"),
                ("images", "images"),
                ("imp", "import"),
                ("inf", "info"),
                ("ins", "inspect"),
                ("k", "kill"),
                ("loa", "load"),
                ("logi", "login"),
                ("logo", "logout"),
                ("logs", "logs"),
                ("m", "manifest"),
                ("ne", "network"),
                ("no", "node"),
                ("pa", "pause"),
                ("pl", "plugin"),
                ("po", "port"),
                ("ps", "ps"),
                ("pul", "pull"),
                ("pus", "push"),
                ("ren", "rename"),
                ("res", "restart"),
                ("rm", "rm"),
                ("rmi", "rmi"),
                ("ru", "run"),
                ("sa", "save"),
                ("sea", "search"),
                ("sec", "secret"),
                ("ser", "service"),
                ("stac", "stack"),
                ("star", "start"),
                ("stat", "stats"),
                ("sto", "stop"),
                ("sw", "swarm"),
                ("sy", "system"),
                ("ta", "tag"),
                ("to", "top"),
                ("tr", "trust"),
                ("un", "unpause"),
                ("up", "update"),
                ("ve", "version"),
                ("vo", "volume"),
                ("w", "wait")
            ]
        );
    }

    #[test]
    fn extract_git() {
        let conf = ConfigFile::from_file(PathBuf::from("git.toml"));
        let (flags, subs) = extract_text(&conf, String::from(GIT_HELP));
        let mut subs = subs
            .iter()
            .map(|s| (s.short.as_str(), s.long.as_str()))
            .collect::<Vec<_>>();
        subs.sort();
        let mut flags = flags
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect::<Vec<_>>();
        flags.sort();
        // TODO: flags
        assert_eq!(flags, [("p", "paginate")]);
        assert_eq!(
            subs.as_slice(),
            [
                ("a", "add"),
                ("am", "am"),
                ("an", "annotate"),
                ("ap", "apply"),
                ("archim", "archimport"),
                ("archiv", "archive"),
                ("bi", "bisect"),
                ("bl", "blame"),
                ("br", "branch"),
                ("bug", "bugreport"),
                ("bun", "bundle"),
                ("ca", "cat-file"),
                ("check-a", "check-attr"),
                ("check-i", "check-ignore"),
                ("check-m", "check-mailmap"),
                ("check-r", "check-ref-format"),
                ("checko", "checkout-index"),
                ("cherry", "cherry"),
                ("cherry-", "cherry-pick"),
                ("ci", "citool"),
                ("cle", "clean"),
                ("clo", "clone"),
                ("cm", "commit"),
                ("co", "checkout"),
                ("col", "column"),
                ("commit-g", "commit-graph"),
                ("commit-t", "commit-tree"),
                ("con", "config"),
                ("cou", "count-objects"),
                ("credential", "credential"),
                ("credential-c", "credential-cache"),
                ("credential-n", "credential-netrc"),
                ("credential-s", "credential-store"),
                ("cvse", "cvsexportcommit"),
                ("cvsi", "cvsimport"),
                ("cvss", "cvsserver"),
                ("da", "daemon"),
                ("de", "describe"),
                ("diff", "diff"),
                ("diff-f", "diff-files"),
                ("diff-i", "diff-index"),
                ("diff-t", "diff-tree"),
                ("difft", "difftool"),
                ("fast-e", "fast-export"),
                ("fast-i", "fast-import"),
                ("fetch", "fetch"),
                ("fetch-", "fetch-pack"),
                ("fi", "filter-branch"),
                ("fm", "fmt-merge-msg"),
                ("for-each-ref", "for-each-ref"),
                ("for-each-rep", "for-each-repo"),
                ("form", "format-patch"),
                ("fs", "fsck"),
                ("gc", "gc"),
                ("ge", "get-tar-commit-id"),
                ("gitk", "gitk"),
                ("gitw", "gitweb"),
                ("gr", "grep"),
                ("gu", "gui"),
                ("ha", "hash-object"),
                ("he", "help"),
                ("ho", "hook"),
                ("ht", "http-backend"),
                ("im", "imap-send"),
                ("ind", "index-pack"),
                ("ini", "init"),
                ("ins", "instaweb"),
                ("int", "interpret-trailers"),
                ("l", "log"),
                ("lf", "lfs"),
                ("ls-f", "ls-files"),
                ("ls-r", "ls-remote"),
                ("ls-t", "ls-tree"),
                ("maili", "mailinfo"),
                ("mails", "mailsplit"),
                ("main", "maintenance"),
                ("merge", "merge"),
                ("merge-b", "merge-base"),
                ("merge-f", "merge-file"),
                ("merge-i", "merge-index"),
                ("merge-o", "merge-one-file"),
                ("merge-t", "merge-tree"),
                ("merget", "mergetool"),
                ("mkta", "mktag"),
                ("mktr", "mktree"),
                ("mu", "multi-pack-index"),
                ("mv", "mv"),
                ("na", "name-rev"),
                ("no", "notes"),
                ("p4", "p4"),
                ("pack-o", "pack-objects"),
                ("pack-red", "pack-redundant"),
                ("pack-ref", "pack-refs"),
                ("pat", "patch-id"),
                ("prune", "prune"),
                ("prune-", "prune-packed"),
                ("pul", "pull"),
                ("pus", "push"),
                ("q", "quiltimport"),
                ("ra", "range-diff"),
                ("rb", "rebase"),
                ("rea", "read-tree"),
                ("ref", "reflog"),
                ("rem", "remote"),
                ("repa", "repack"),
                ("repl", "replace"),
                ("req", "request-pull"),
                ("rer", "rerere"),
                ("rese", "reset"),
                ("rest", "restore"),
                ("rev-l", "rev-list"),
                ("rev-p", "rev-parse"),
                ("reve", "revert"),
                ("rm", "rm"),
                ("send-e", "send-email"),
                ("send-p", "send-pack"),
                ("sh-i", "sh-i18n"),
                ("sh-s", "sh-setup"),
                ("shor", "shortlog"),
                ("show", "show"),
                ("show-b", "show-branch"),
                ("show-i", "show-index"),
                ("show-r", "show-ref"),
                ("sp", "sparse-checkout"),
                ("stas", "stash"),
                ("stat", "status"),
                ("str", "stripspace"),
                ("su", "submodule"),
                ("sv", "svn"),
                ("sw", "switch"),
                ("sy", "symbolic-ref"),
                ("t", "tag"),
                ("unpack-f", "unpack-file"),
                ("unpack-o", "unpack-objects"),
                ("update-i", "update-index"),
                ("update-r", "update-ref"),
                ("update-s", "update-server-info"),
                ("va", "var"),
                ("verify-c", "verify-commit"),
                ("verify-p", "verify-pack"),
                ("verify-t", "verify-tag"),
                ("wh", "whatchanged"),
                ("wo", "worktree"),
                ("wr", "write-tree")
            ]
        );
    }
}
