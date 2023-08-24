use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::Path,
    process::Command,
};

use tracing::debug;

/// For use with serde's [serialize_with] attribute
fn ordered_map<S: serde::Serializer, K: Ord + serde::Serialize, V: serde::Serialize>(
    value: &HashMap<K, V>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    let ordered: BTreeMap<_, _> = value.iter().collect();
    serde::Serialize::serialize(&ordered, serializer)
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
pub(super) struct Cmds(#[serde(serialize_with = "ordered_map")] pub(super) HashMap<String, Cmd>);

impl Cmds {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub(super) struct Cmd {
    pub(super) short: String,
    #[serde(default)]
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    #[serde(serialize_with = "ordered_map")]
    pub(super) flags: HashMap<String, String>,
    #[serde(default)]
    #[serde(skip_serializing_if = "Cmds::is_empty")]
    pub(super) subs: Cmds,
}

#[derive(Clone, Debug, Default, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ConfigFile {
    short: Option<String>,
    #[serde(default)]
    devowel: bool,
    #[serde(default)]
    extra_subs: Vec<String>,
    #[serde(default)]
    flags: HashMap<String, String>,
    #[serde(default)]
    stop: bool,
    #[serde(default)]
    subs: HashMap<String, ConfigFile>,
}

impl ConfigFile {
    pub(super) fn from_file<P: AsRef<Path>>(p: P) -> Self {
        toml::from_str::<ConfigFile>(&std::fs::read_to_string(p).unwrap()).unwrap()
    }
}

fn unique_prefixes(strings: &[String], denylist: &HashSet<&str>) -> HashMap<String, String> {
    let mut result = HashMap::with_capacity(strings.len());
    for string in strings.iter() {
        let mut pfx = String::new();
        for c in string.chars() {
            pfx.push(c);
            if denylist.contains(pfx.as_str()) {
                continue;
            }
            let is_unique = strings.iter().filter(|s| s.starts_with(&pfx)).count() == 1;
            if is_unique {
                break;
            }
        }
        result.insert(string.clone(), pfx);
    }
    result
}

fn is_vowel(c: char) -> bool {
    c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'
}

fn remove_vowels(strings: &[String]) -> HashMap<&str, String> {
    let mut result = HashMap::with_capacity(strings.len());
    let mut seen = HashSet::with_capacity(strings.len());
    for string in strings.iter() {
        if string.is_empty() {
            continue;
        }
        let first = string.chars().next().unwrap();
        if is_vowel(first) {
            continue;
        }
        let vowelless = string.chars().filter(|c| !is_vowel(*c)).collect::<String>();
        if seen.contains(&vowelless) {
            result.remove(string.as_str());
            continue;
        }
        result.insert(string.as_str(), vowelless.clone());
        seen.insert(vowelless);
    }
    result
}

fn do_remove_vowels(strings: &[String]) -> Vec<String> {
    let mut result = Vec::with_capacity(strings.len());
    let mut shortened = remove_vowels(strings);
    for s in strings {
        result.push(if let Some(shorter) = shortened.remove(s.as_str()) {
            shorter
        } else {
            s.clone()
        });
    }
    result
}

// TODO: Optionally add a prefix '-' to all short versions of flags
fn deconflict(conf: &ConfigFile, flags: &[String], subs: &[String]) -> HashMap<String, String> {
    debug_assert!(flags.len() == HashSet::<&String>::from_iter(flags).len());
    debug_assert!(subs.len() == HashSet::<&String>::from_iter(subs).len());

    let total_len = flags.len() + subs.len();
    let mut result = HashMap::with_capacity(total_len);
    let mut rest = Vec::with_capacity(total_len - conf.flags.len());
    let mut denylist = HashSet::with_capacity(conf.flags.len());

    for flag in flags.iter() {
        if let Some(short) = conf.flags.get(flag) {
            debug_assert!(result.get(flag).is_none());
            result.insert(flag.clone(), short.clone());
            debug_assert!(!denylist.contains(short.as_str()));
            denylist.insert(short.as_str());
        } else {
            debug_assert!(!rest.contains(flag));
            rest.push(flag.clone());
        }
    }
    for sub in subs.iter() {
        if let Some(sub_conf) = conf.subs.get(sub) {
            if let Some(short) = &sub_conf.short {
                debug_assert!(result.get(sub).is_none());
                result.insert(sub.clone(), short.clone());
                debug_assert!(!denylist.contains(short.as_str()));
                denylist.insert(short.as_str());
            } else {
                // TODO: How to account for flags and subcommands with the same name?
                // debug_assert!(!rest.contains(sub));
                rest.push(sub.clone());
            }
        } else {
            // TODO: How to account for flags and subcommands with the same name?
            // debug_assert!(!rest.contains(sub));
            rest.push(sub.clone());
        }
    }

    if conf.devowel {
        let rmvd = do_remove_vowels(rest.as_slice());
        let pfxs = unique_prefixes(&rmvd, &denylist);
        debug_assert_eq!(rmvd.len(), rest.len());
        for (rmd, long) in rmvd.iter().zip(rest.iter()) {
            result.insert(long.clone(), pfxs.get(rmd).unwrap().clone());
        }
    } else {
        result.extend(unique_prefixes(&rest, &denylist));
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
        let long = first
            .chars()
            .filter(|c| *c == '-' || c.is_alphanumeric())
            .collect::<String>();
        return Some(long);
    }
    None
}

#[derive(Debug, Eq, Hash, PartialEq, PartialOrd)]
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
        if !first.starts_with(LONG) || first.len() <= LONG.len() {
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
            .filter(|c| *c == '-' || c.is_alphanumeric())
            .collect::<String>();
        if long.len() <= 2 {
            continue;
        }
        return Some(Opt { short, long });
    }
    None
}

#[allow(dead_code)]
fn extract_text(conf: &ConfigFile, text: String) -> (HashMap<String, String>, Cmds) {
    let mut opts = HashSet::new();
    let mut sub_names = HashSet::<String>::from_iter(conf.extra_subs.iter().cloned());
    for mut line in text.lines() {
        if !line.starts_with([' ', ' ']) {
            continue;
        }
        line = line.trim_start();
        let words = line.split_whitespace().collect::<Vec<_>>();

        if let Some(long) = extract_sub(words.as_slice()) {
            sub_names.insert(long);
        }

        if !line.contains(['-', '-']) {
            continue;
        }
        if let Some(opt) = extract_opt(words.as_slice()) {
            opts.insert(opt);
        }
    }

    let sub_name_vec = Vec::from_iter(sub_names.iter().cloned());
    let opt_names = opts.into_iter().map(|o| o.long).collect::<Vec<_>>();
    let deconflicted = deconflict(conf, opt_names.as_slice(), sub_name_vec.as_slice());

    let mut subs = Cmds(HashMap::with_capacity(sub_names.len()));
    for long in sub_names {
        let short = deconflicted.get(&long).unwrap().clone();
        if long == short {
            debug!("Couldn't abbreviate {short}");
        }
        subs.0.insert(
            long,
            Cmd {
                short,
                flags: HashMap::new(),
                subs: Cmds::default(),
            },
        );
    }

    let mut flags = HashMap::<String, String>::new();
    for long in opt_names {
        let short = deconflicted.get(&long).unwrap().clone();
        if long == short {
            debug!("Couldn't abbreviate {short}");
        }
        if let Some(v) = flags.get(&short) {
            assert_eq!(*v, long);
        }
        flags.insert(short, long);
    }
    subs = if conf.stop { Cmds::default() } else { subs };
    (flags, subs)
}

fn help(args: &[String]) -> Option<String> {
    let mut builder = Command::new(&args[0]);
    builder.args(&args[1..]).arg("--help");
    debug!("Running {builder:?}");
    let output = builder.output().unwrap();
    if !output.status.success() {
        return None;
    }
    String::from_utf8(output.stdout).ok()
}

pub(super) fn extract_recursive(
    mut prefix: Vec<String>,
    conf: ConfigFile,
    long: String,
) -> Option<Cmd> {
    prefix.push(long.clone());
    let h = help(&prefix)?;
    let (flags, subs0) = extract_text(&conf, h);

    let mut subs = Cmds(HashMap::with_capacity(subs0.0.len()));
    // TODO: Fix enough bugs to allow recursion
    if prefix.is_empty() {
        for (long, sub0) in subs0.0 {
            let sub_conf = conf.subs.get(&long).cloned().unwrap_or_default();
            if let Some(mut sub) = extract_recursive(prefix.clone(), sub_conf, long.clone()) {
                sub.short = sub0.short; // already deconflicted
                subs.0.insert(long, sub);
            }
        }
    } else {
        subs.0.extend(subs0.0);
    }

    Some(Cmd {
        short: conf
            .short
            .unwrap_or_else(|| String::from(long.chars().next().unwrap())),
        flags,
        subs,
    })
}

// TODO recur
pub(super) fn extract(conf: ConfigFile, long: String) -> Option<Cmd> {
    extract_recursive(Vec::new(), conf, long)
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, path::PathBuf};

    use super::{deconflict, extract_text, ConfigFile};

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

    const GLAB_HELP: &str = "
GLab is an open source GitLab CLI tool bringing GitLab to your command line

USAGE
  glab <command> <subcommand> [flags]

CORE COMMANDS
  alias:       Create, list and delete aliases
  api:         Make an authenticated request to GitLab API
  auth:        Manage glab's authentication state
  check-update: Check for latest glab releases
  ci:          Work with GitLab CI pipelines and jobs
  completion:  Generate shell completion scripts
  config:      Set and get glab settings
  help:        Help about any command
  issue:       Work with GitLab issues
  label:       Manage labels on remote
  mr:          Create, view and manage merge requests
  release:     Manage GitLab releases
  repo:        Work with GitLab repositories and projects
  ssh-key:     Manage SSH keys
  user:        Interact with user
  variable:    Manage GitLab Project and Group Variables
  version:     show glab version information

FLAGS
      --help      Show help for command
  -v, --version   show glab version information

ENVIRONMENT VARIABLES
  GITLAB_TOKEN: an authentication token for API requests. Setting this avoids being
  prompted to authenticate and overrides any previously stored credentials.
  Can be set in the config with 'glab config set token xxxxxx'
  
  GITLAB_HOST or GL_HOST: specify the url of the gitlab server if self hosted (eg: https://gitlab.example.com). Default is https://gitlab.com.
  
  REMOTE_ALIAS or GIT_REMOTE_URL_VAR: git remote variable or alias that contains the gitlab url.
  Can be set in the config with 'glab config set remote_alias origin'
  
  VISUAL, EDITOR (in order of precedence): the editor tool to use for authoring text.
  Can be set in the config with 'glab config set editor vim'
  
  BROWSER: the web browser to use for opening links.
  Can be set in the config with 'glab config set browser mybrowser'
  
  GLAMOUR_STYLE: environment variable to set your desired markdown renderer style
  Available options are (dark|light|notty) or set a custom style
  https://github.com/charmbracelet/glamour#styles
  
  NO_PROMPT: set to 1 (true) or 0 (false) to disable and enable prompts respectively
  
  NO_COLOR: set to any value to avoid printing ANSI escape sequences for color output.
  
  FORCE_HYPERLINKS: set to 1 to force hyperlinks to be output, even when not outputing to a TTY
  
  GLAB_CONFIG_DIR: set to a directory path to override the global configuration location 

LEARN MORE
  Use 'glab <command> <subcommand> --help' for more information about a command.

FEEDBACK
  Encountered a bug or want to suggest a feature?
  Open an issue using 'glab issue create -R profclems/glab'";

    fn go(conf: &ConfigFile, s: String) -> (Vec<(String, String)>, Vec<(String, String)>) {
        let (flags, subs) = extract_text(conf, s);
        let mut subs = subs
            .0
            .iter()
            .map(|(long, s)| (s.short.clone(), long.clone()))
            .collect::<Vec<_>>();
        subs.sort();
        let mut flags = flags
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<Vec<_>>();
        flags.sort();
        (flags, subs)
    }

    #[test]
    fn extract_cabal() {
        let (flags, subs) = go(&ConfigFile::default(), String::from(CABAL_HELP));
        assert_eq!(
            flags
                .iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
            [
                ("a", "active-repositories"),
                ("config-", "config-file"),
                ("d", "disable-nix"),
                ("en", "enable-nix"),
                ("help", "help"),
                ("ht", "http-transport"),
                ("ig", "ignore-expiry"),
                ("ni", "nix"),
                ("nu", "numeric-version"),
                ("st", "store-dir"),
                ("ve", "version")
            ]
        );
        assert_eq!(
            subs.iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
            [
                ("be", "bench"),
                ("bu", "build"),
                ("ca", "cabal"),
                ("ch", "check"),
                ("cl", "clean"),
                ("configu", "configure"),
                ("ex", "exec"),
                ("fe", "fetch"),
                ("fr", "freeze"),
                ("gen", "gen-bounds"),
                ("get", "get"),
                ("haddock", "haddock"),
                ("haddock-", "haddock-project"),
                ("help", "help"),
                ("hs", "hscolour"),
                ("inf", "info"),
                ("ini", "init"),
                ("ins", "install"),
                ("list", "list"),
                ("list-", "list-bin"),
                ("ne", "new-haddock-project"),
                ("o", "outdated"),
                ("repl", "repl"),
                ("repo", "report"),
                ("ru", "run"),
                ("sd", "sdist"),
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
        let (flags, subs) = go(&ConfigFile::default(), String::from(CARGO_HELP));
        assert_eq!(
            flags
                .iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
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
            subs.iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
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
        let (flags, subs) = go(&ConfigFile::default(), String::from(DOCKER_HELP));
        assert_eq!(
            flags
                .iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
            [
                ("config", "config"),
                ("context", "context"),
                ("de", "debug"),
                ("ho", "host"),
                ("log-", "log-level"),
                ("tls", "tls"),
                ("tlsca", "tlscacert"),
                ("tlsce", "tlscert"),
                ("tlsk", "tlskey"),
                ("tlsv", "tlsverify"),
                ("version", "version")
            ]
        );
        assert_eq!(
            subs.iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
            [
                ("a", "attach"),
                ("build", "build"),
                ("builde", "builder"),
                ("buildx", "buildx"),
                ("comm", "commit"),
                ("comp", "compose"),
                ("config", "config"),
                ("conta", "container"),
                ("context", "context"),
                ("cp", "cp"),
                ("cr", "create"),
                ("di", "diff"),
                ("ev", "events"),
                ("exe", "exec"),
                ("exp", "export"),
                ("hi", "history"),
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
                ("version", "version"),
                ("vo", "volume"),
                ("w", "wait")
            ]
        );
    }

    #[test]
    fn extract_git() {
        let conf = ConfigFile::from_file(PathBuf::from("git.toml"));
        let (flags, subs) = go(&conf, String::from(GIT_HELP));
        // TODO: flags
        assert_eq!(
            flags
                .iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
            [("pag", "paginate")]
        );
        assert_eq!(
            subs.iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
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
                ("s", "status"),
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
                ("sta", "stash"),
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

    #[test]
    fn extract_glab() {
        let (flags, subs) = go(&ConfigFile::default(), String::from(GLAB_HELP));
        assert_eq!(
            flags
                .iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
            [("help", "help"), ("ve", "version")]
        );
        assert_eq!(
            subs.iter()
                .map(|(k, v)| (k.as_str(), v.as_str()))
                .collect::<Vec<_>>(),
            [
                ("al", "alias"),
                ("ap", "api"),
                ("au", "auth"),
                ("ch", "check-update"),
                ("ci", "ci"),
                ("com", "completion"),
                ("con", "config"),
                ("help", "help"),
                ("i", "issue"),
                ("l", "label"),
                ("m", "mr"),
                ("rel", "release"),
                ("rep", "repo"),
                ("s", "ssh-key"),
                ("u", "user"),
                ("va", "variable")
            ]
        );
    }

    #[test]
    fn test_deconflict() {
        let conf = ConfigFile::default();
        assert_eq!(deconflict(&conf, &[], &[]), HashMap::new());
        assert_eq!(
            deconflict(&conf, &[String::from("foo")], &[]),
            HashMap::from([(String::from("foo"), String::from("f"))])
        );
        assert_eq!(
            deconflict(&conf, &[String::from("bar"), String::from("baz")], &[]),
            HashMap::from([
                (String::from("bar"), String::from("bar")),
                (String::from("baz"), String::from("baz"))
            ])
        );
        let conf = ConfigFile::from_file(PathBuf::from("git.toml"));
        assert_eq!(
            deconflict(&conf, &[String::from("show"), String::from("status")], &[]),
            HashMap::from([
                (String::from("status"), String::from("st")),
                (String::from("show"), String::from("sh"))
            ])
        );
    }
}
