use std::{
    env,
    ffi::OsStr,
    fs,
    io::{self, BufRead as _},
    path::Path,
    process::Command,
};

use crate::system;

#[derive(Clone, Debug)]
pub(crate) enum Cmd {
    Cmd {
        bin: &'static str,
        args: &'static [&'static str],
        glob: Option<&'static str>,
    },
    Shell(String),
}

const HLINT: Cmd = Cmd::Cmd {
    bin: "hlint",
    args: &[],
    glob: Some("*.hs"),
};

const FOURMOLU: Cmd = Cmd::Cmd {
    bin: "fourmolu",
    args: &["--mode", "inplace"],
    glob: Some("*.hs"),
};

fn quote(s: String) -> String {
    if s.contains(' ') {
        format!("\"{s}\"")
    } else {
        s
    }
}

pub(crate) fn print_cmd(cmd: &Command) -> String {
    if cmd.get_program() == OsStr::new("bash") && cmd.get_args().next() == Some(OsStr::new("-c")) {
        cmd.get_args()
            .skip(1)
            .map(|s| s.display().to_string())
            .collect::<Vec<_>>()
            .join(" ")
    } else {
        format!(
            "{} {}",
            cmd.get_program().display(),
            cmd.get_args()
                .map(|s| quote(s.display().to_string()))
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

impl Cmd {
    pub(crate) fn to_command(&self, precommit: bool) -> Command {
        match self {
            Cmd::Cmd {
                bin,
                args,
                glob: None,
            } => {
                let mut cmd = Command::new(*bin);
                cmd.args(*args);
                cmd
            }
            Cmd::Cmd {
                bin,
                args,
                glob: Some(g),
            } => {
                let mut cmd = Command::new("bash");
                cmd.arg("-c");
                if precommit {
                    cmd.arg(format!(
                        "git diff -z --diff-filter=d --name-only --cached -- '{g}' | xargs -0 {bin} {}",
                        args.join(" ")
                    ));
                } else {
                    cmd.arg(format!(
                        "git ls-files -z --exclude-standard '{g}' | xargs -0 {bin} {}",
                        args.join(" ")
                    ));
                }
                cmd
            }
            Cmd::Shell(script) => {
                let mut cmd = Command::new("bash");
                cmd.args(["-c", script.as_str()]);
                cmd
            }
        }
    }

    fn to_command_line(&self, precommit: bool) -> String {
        let cmd = self.to_command(precommit);
        print_cmd(&cmd)
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Project {
    pub(crate) name: &'static str,
    pub(crate) lint: Option<Cmd>,
    pub(crate) format: Option<Cmd>,
    pub(crate) fix: Option<Cmd>,
    pub(crate) build: Option<Cmd>,
    pub(crate) test: Option<Cmd>,
    pub(crate) run: Option<Cmd>,
    pub(crate) watch: Option<Cmd>,
    pub(crate) aliases: &'static [(&'static str, &'static str)],
}

#[expect(dead_code)]
#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
pub(crate) enum Confidence {
    Low,
    Med,
    High,
}

pub(crate) fn mine() -> bool {
    if let Ok(f) = fs::File::open("LICENSE") {
        let reader = io::BufReader::new(f);
        for (i, line) in reader.lines().enumerate() {
            if let Ok(line) = line {
                if line.contains("Barrett") {
                    return true;
                } else if line.contains("Galois") {
                    return false;
                }
            }
            if i >= 3 {
                break;
            }
        }
    }
    if Path::new("scripts/lint/lint.py").exists() {
        return true;
    }
    let mut cmd = Command::new("gh");
    cmd.args(["repo", "view", "--json", "owner", "--jq", ".owner.login"]);
    let out = {
        let Ok(out) = cmd.output() else {
            return false;
        };
        if !out.status.success() {
            return false;
        }
        out
    };
    let owner = String::from_utf8_lossy(&out.stdout);
    owner.trim() == "langston-barrett"
}

impl Project {
    pub(crate) fn infer(&mut self, min_confidence: Confidence) {
        let Ok(pwd) = env::current_dir() else {
            return;
        };
        let build_system = system::System::detect(&pwd);
        let has_lint_py = Path::new("scripts/lint/lint.py").exists();
        self.infer_lint(build_system, min_confidence, has_lint_py);
        self.infer_format(build_system, min_confidence, has_lint_py);
        // TODO: continue integrating `min_confidence`
        self.infer_fix(build_system, has_lint_py);
        self.infer_build(build_system);
        self.infer_test(build_system);
        self.infer_run(build_system);
        self.infer_watch(build_system);
    }

    fn infer_lint(
        &mut self,
        build_system: Option<system::System>,
        min_confidence: Confidence,
        has_lint_py: bool,
    ) {
        if self.lint.is_some() {
            return;
        }
        self.lint = if has_lint_py {
            Some(Cmd::Cmd {
                bin: "scripts/lint/lint.py",
                args: &[],
                glob: None,
            })
        } else if (Path::new(".hlint.yml").exists() || Path::new(".hlint.yaml").exists())
            && min_confidence <= Confidence::High
        {
            Some(HLINT)
        } else {
            match build_system {
                Some(system::System::Cargo) if min_confidence <= Confidence::High => {
                    Some(Cmd::Shell(
                        "cargo fmt --check && cargo clippy --all-targets -- --deny warnings"
                            .to_string(),
                    ))
                }
                Some(system::System::Make) if min_confidence <= Confidence::Low => Some(Cmd::Cmd {
                    bin: "make",
                    args: &["lint"],
                    glob: None,
                }),
                _ => None,
            }
        }
    }

    fn infer_format(
        &mut self,
        build_system: Option<system::System>,
        min_confidence: Confidence,
        has_lint_py: bool,
    ) {
        if self.format.is_some() {
            return;
        }
        self.format = if has_lint_py {
            Some(Cmd::Cmd {
                bin: "scripts/lint/lint.py",
                args: &["--format"],
                glob: None,
            })
        } else if (Path::new("fourmolu.yml").exists() || Path::new("fourmolu.yaml").exists())
            && min_confidence <= Confidence::High
        {
            Some(FOURMOLU)
        } else {
            match build_system {
                Some(system::System::Cabal) if min_confidence <= Confidence::Low => Some(FOURMOLU),
                Some(system::System::Cargo) if min_confidence <= Confidence::High => {
                    Some(Cmd::Cmd {
                        bin: "cargo",
                        args: &["fmt"],
                        glob: None,
                    })
                }
                Some(system::System::Make) if min_confidence <= Confidence::Low => Some(Cmd::Cmd {
                    bin: "make",
                    args: &["fmt"],
                    glob: None,
                }),
                _ => None,
            }
        }
    }

    fn infer_fix(&mut self, build_system: Option<system::System>, has_lint_py: bool) {
        if self.fix.is_some() {
            return;
        }

        if has_lint_py {
            self.fix = Some(Cmd::Cmd {
                bin: "scripts/lint/lint.py",
                args: &["--fix"],
                glob: None,
            });
        } else {
            self.fix = match build_system {
                Some(system::System::Cargo) => Some(Cmd::Cmd {
                    bin: "cargo",
                    args: &[
                        "clippy",
                        "--allow-dirty",
                        "--fix",
                        "--",
                        "--deny",
                        "warnings",
                    ],
                    glob: None,
                }),
                _ => None,
            };
        }
    }

    fn infer_build(&mut self, build_system: Option<system::System>) {
        if self.build.is_some() {
            return;
        }
        self.build = match build_system {
            Some(system::System::Cabal) => Some(Cmd::Cmd {
                bin: "cabal",
                args: &["build"],
                glob: None,
            }),
            Some(system::System::Cargo) => Some(Cmd::Cmd {
                bin: "cargo",
                args: &["build"],
                glob: None,
            }),
            Some(system::System::Make) => Some(Cmd::Cmd {
                bin: "make",
                args: &[],
                glob: None,
            }),
            None => None,
        };
    }

    fn infer_test(&mut self, build_system: Option<system::System>) {
        if self.test.is_some() {
            return;
        }
        self.test = match build_system {
            Some(system::System::Cabal) => Some(Cmd::Cmd {
                bin: "cabal",
                args: &["test"],
                glob: None,
            }),
            Some(system::System::Cargo) => Some(Cmd::Cmd {
                bin: "cargo",
                args: &["test"],
                glob: None,
            }),
            Some(system::System::Make) => Some(Cmd::Cmd {
                bin: "make",
                args: &["test"],
                glob: None,
            }),
            None => None,
        };
    }

    fn infer_run(&mut self, build_system: Option<system::System>) {
        if self.run.is_some() {
            return;
        }
        self.run = match build_system {
            Some(system::System::Cabal) => Some(Cmd::Cmd {
                bin: "cabal",
                args: &["run"],
                glob: None,
            }),
            Some(system::System::Cargo) => Some(Cmd::Cmd {
                bin: "cargo",
                args: &["run"],
                glob: None,
            }),
            Some(system::System::Make) | None => None,
        };
    }

    fn infer_watch(&mut self, build_system: Option<system::System>) {
        if self.watch.is_some() {
            return;
        }
        self.watch = match build_system {
            Some(system::System::Cabal) => Some(Cmd::Cmd {
                bin: "ghcid",
                args: &[],
                glob: None,
            }),
            _ => {
                if let (Some(format_cmd), Some(lint_cmd)) = (&self.format, &self.lint) {
                    let fmt_str = format_cmd.to_command_line(false);
                    let lint_str = lint_cmd.to_command_line(false);
                    Some(Cmd::Shell(format!(
                        "git ls-files --exclude-standard | entr -c -s '{fmt_str} && {lint_str}'"
                    )))
                } else if let Some(format_cmd) = &self.format {
                    let fmt_str = format_cmd.to_command_line(false);
                    Some(Cmd::Shell(format!(
                        "git ls-files --exclude-standard | entr -c -s '{fmt_str}'"
                    )))
                } else if let Some(lint_cmd) = &self.lint {
                    let lint_str = lint_cmd.to_command_line(false);
                    Some(Cmd::Shell(format!(
                        "git ls-files --exclude-standard | entr -c -s '{lint_str}'"
                    )))
                } else {
                    None
                }
            }
        };
    }
}

const CRUCIBLE_LLVM_CLI: Project = Project {
    name: "crucible-llvm-cli",
    lint: None,
    format: None,
    fix: None,
    build: None,
    test: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "test:crucible-llvm-cli-tests", "--"],
        glob: None,
    }),
    run: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "exe:crucible-llvm", "--"],
        glob: None,
    }),
    watch: Some(Cmd::Cmd {
        bin: "ghcid",
        args: &[],
        glob: None,
    }),
    aliases: &[
        ("rs", "cabal run exe:crucible-llvm -- simulate"),
        ("wt", "ghcid --target=test:crucible-llvm-cli-tests"),
    ],
};

const DETECT: Project = Project {
    name: "detect",
    lint: None,
    format: None,
    fix: None,
    build: None,
    test: None,
    run: None,
    watch: None,
    aliases: &[
        (
            "bs",
            "echo 1 | sudo tee /proc/sys/kernel/perf_event_paranoid && sudo sysctl kernel.perf_event_mlock_kb=2048 && cargo b -q --profile=profiling --bin=sofuzz && samply record ./target/profiling/sofuzz --solutions /run/user/1000/sols --gas=2048 sofuzz/rs/map/map.toml target/profiling/libsofuzz_map.so --no-check-dwarf",
        ),
        (
            "e1",
            "rm -rf benign solutions ; cargo build -p=eval1-smi-model && cargo run --bin dxezz -- --qcow=targets/eval1-smi/image-debug/snapshots.qcow2 targets/eval1-smi/eval1-smi-debug.toml target/debug/libeval1_smi_model.so --seed=1 --outer-iterations=8 --inner-iterations=1 --no-check-snapshots -v",
        ),
        (
            "lu",
            "cargo clippy --all-targets --no-default-features --features=usermode --target-dir=target-usermode -- --deny warnings",
        ),
        ("rb", "cargo run --bin=bzro --"),
        (
            "rbu",
            "cargo run --bin=bzro --no-default-features --features=usermode --target-dir=target-usermode --",
        ),
        ("rd", "cargo run --bin=dxezz --"),
        ("rs", "cargo run --bin=sofuzz --"),
        (
            "tu",
            "cargo test --no-default-features --features=usermode --target-dir=target-usermode",
        ),
        ("tb", "cargo test --package=bzro -- --test-threads=1"),
        (
            "tbu",
            "cargo test --package=bzro --no-default-features --features=usermode --target-dir=target-usermode -- --test-threads=1",
        ),
        ("td", "cargo test --package=dxezz -- --test-threads=1"),
        (
            "ts",
            "cargo b -q --package=sofuzz-boxcar && cargo b -q --package=sofuzz-map && cargo test --package=sofuzz",
        ),
    ],
};

const GREASE: Project = Project {
    name: "grease",
    lint: Some(Cmd::Cmd {
        bin: "make",
        args: &[
            "-j8",
            "-f",
            "scripts/lint/Makefile",
            "hs",
            "make",
            "md",
            "py",
            "sh",
            "merge",
            "whitespace",
        ],
        glob: None,
    }),
    format: Some(Cmd::Cmd {
        bin: "make",
        args: &["-j8", "-f", "scripts/lint/Makefile", "fmt"],
        glob: None,
    }),
    fix: None,
    build: None, // can be guessed from `.cabal`
    test: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "test:grease-tests", "--"],
        glob: None,
    }),
    run: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "exe:grease", "--"],
        glob: None,
    }),
    watch: Some(Cmd::Cmd {
        bin: "ghcid",
        args: &[
            "--command",
            "cabal repl lib:grease pkg:grease-cli pkg:grease-exe test:grease-tests",
        ],
        glob: None,
    }),
    aliases: &[
        (
            "to",
            "cabal run exe:grease -- --symbol test $(fd --type=x elf tests/ | pick)",
        ),
        ("wt", "ghcid --target=test:grease-tests"),
    ],
};

const SCREACH: Project = Project {
    name: "screach",
    lint: Some(Cmd::Cmd {
        bin: "hlint",
        args: &[
            "--hint=../deps/grease/.hlint.yaml",
            "{app,src,test}",
            "../elf-edit-ecfs/{src,tools}",
        ],
        glob: None,
    }),
    format: None,
    fix: None,
    build: None,
    test: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "test:screach-test", "--"],
        glob: None,
    }),
    run: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "exe:screach", "--"],
        glob: None,
    }),
    watch: Some(Cmd::Cmd {
        bin: "ghcid",
        args: &["--command", "cabal repl lib:screach exe:screach"],
        glob: None,
    }),
    aliases: &[("wt", "ghcid --target=test:screach-test")],
};

pub(crate) const PROJECTS: &[Project] = &[CRUCIBLE_LLVM_CLI, DETECT, GREASE, SCREACH];

pub(crate) fn git_root_name() -> Option<String> {
    let output = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let root_path = String::from_utf8(output.stdout).ok()?.trim().to_string();
    Path::new(&root_path)
        .file_name()
        .and_then(|n| n.to_str())
        .map(ToString::to_string)
}

pub(crate) fn project() -> Option<&'static Project> {
    let current = env::current_dir().ok()?;
    let current = current.file_name().and_then(|f| f.to_str())?;
    PROJECTS
        .iter()
        .find(|p| p.name == current)
        .or_else(|| git_root_name().and_then(|name| PROJECTS.iter().find(|p| p.name == name)))
}

pub(crate) fn project_expansions(project: &Project) -> Vec<(&'static str, String)> {
    let mut expansions: Vec<(&str, String)> = Vec::new();

    if let Some(cmd) = &project.lint {
        expansions.push(("l", cmd.to_command_line(false)));
    }
    if let Some(cmd) = &project.format {
        expansions.push(("f", cmd.to_command_line(false)));
    }
    if let Some(cmd) = &project.fix {
        expansions.push(("fix", cmd.to_command_line(false)));
    }
    if let Some(fix) = &project.fix
        && let Some(fmt) = &project.format
    {
        expansions.push((
            "ff",
            format!(
                "{} && {}",
                fmt.to_command_line(false),
                fix.to_command_line(false)
            ),
        ));
    }
    if let Some(cmd) = &project.build {
        expansions.push(("b", cmd.to_command_line(false)));
    }
    if let Some(cmd) = &project.test {
        expansions.push(("t", cmd.to_command_line(false)));
    }
    if let Some(cmd) = &project.run {
        expansions.push(("r", cmd.to_command_line(false)));
    }
    if let Some(cmd) = &project.watch {
        expansions.push(("w", cmd.to_command_line(false)));
    }
    for (shortcut, command) in project.aliases.iter().copied() {
        expansions.push((shortcut, command.to_string()));
    }
    expansions
}

pub(super) fn go() -> anyhow::Result<()> {
    let Some(project) = project() else {
        return Ok(());
    };
    for (short, long) in project_expansions(project) {
        println!("{short} --> {long}");
    }

    Ok(())
}
