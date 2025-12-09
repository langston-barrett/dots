use std::{env, path::Path, process::Command};

use crate::system;

#[derive(Clone, Debug)]
pub(crate) enum Cmd {
    Cmd {
        bin: &'static str,
        args: &'static [&'static str],
    },
    Shell(String),
}

impl Cmd {
    pub(crate) fn to_command(&self) -> Command {
        match self {
            Cmd::Cmd { bin, args } => {
                let mut cmd = Command::new(*bin);
                cmd.args(*args);
                cmd
            }
            Cmd::Shell(script) => {
                let mut cmd = Command::new("bash");
                cmd.args(["-c", script.as_str()]);
                cmd
            }
        }
    }

    fn to_command_line(&self) -> String {
        match self {
            Cmd::Cmd { bin, args } => {
                if args.is_empty() {
                    (*bin).to_owned()
                } else {
                    format!("{} {}", bin, args.join(" "))
                }
            }
            Cmd::Shell(script) => script.clone(),
        }
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

impl Project {
    pub(crate) fn infer(&mut self) {
        let Ok(pwd) = env::current_dir() else {
            return;
        };
        let build_system = system::System::detect(&pwd);
        let has_lint_py = Path::new("scripts/lint/lint.py").exists();
        self.infer_lint(build_system, has_lint_py);
        self.infer_format(build_system, has_lint_py);
        self.infer_fix(build_system, has_lint_py);
        self.infer_build(build_system);
        self.infer_test(build_system);
        self.infer_run(build_system);
        self.infer_watch(build_system);
    }

    fn infer_lint(&mut self, build_system: Option<system::System>, has_lint_py: bool) {
        if self.lint.is_none() {
            if has_lint_py {
                self.lint = Some(Cmd::Cmd {
                    bin: "scripts/lint/lint.py",
                    args: &[],
                });
            } else {
                self.lint = match build_system {
                    Some(system::System::Cargo) => Some(Cmd::Shell(
                        "cargo fmt --check && cargo clippy --all-targets -- --deny warnings"
                            .to_string(),
                    )),
                    Some(system::System::Make) => Some(Cmd::Cmd {
                        bin: "make",
                        args: &["lint"],
                    }),
                    _ => None,
                };
            }
        }
    }

    fn infer_format(&mut self, build_system: Option<system::System>, has_lint_py: bool) {
        if self.format.is_none() {
            if has_lint_py {
                self.format = Some(Cmd::Cmd {
                    bin: "scripts/lint/lint.py",
                    args: &["--format"],
                });
            } else {
                self.format = match build_system {
                    Some(system::System::Cabal) => Some(Cmd::Shell(
                        "git ls-files -z --exclude-standard | xargs -0 fourmolu --mode inplace"
                            .to_string(),
                    )),
                    Some(system::System::Cargo) => Some(Cmd::Cmd {
                        bin: "cargo",
                        args: &["fmt"],
                    }),
                    Some(system::System::Make) => Some(Cmd::Cmd {
                        bin: "make",
                        args: &["fmt"],
                    }),
                    None => None,
                };
            }
        }
    }

    fn infer_fix(&mut self, build_system: Option<system::System>, has_lint_py: bool) {
        if self.fix.is_none() {
            if has_lint_py {
                self.fix = Some(Cmd::Cmd {
                    bin: "scripts/lint/lint.py",
                    args: &["--fix"],
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
                    }),
                    _ => None,
                };
            }
        }
    }

    fn infer_build(&mut self, build_system: Option<system::System>) {
        if self.build.is_none() {
            self.build = match build_system {
                Some(system::System::Cabal) => Some(Cmd::Cmd {
                    bin: "cabal",
                    args: &["build"],
                }),
                Some(system::System::Cargo) => Some(Cmd::Cmd {
                    bin: "cargo",
                    args: &["build"],
                }),
                Some(system::System::Make) => Some(Cmd::Cmd {
                    bin: "make",
                    args: &[],
                }),
                None => None,
            };
        }
    }

    fn infer_test(&mut self, build_system: Option<system::System>) {
        if self.test.is_none() {
            self.test = match build_system {
                Some(system::System::Cabal) => Some(Cmd::Cmd {
                    bin: "cabal",
                    args: &["test"],
                }),
                Some(system::System::Cargo) => Some(Cmd::Cmd {
                    bin: "cargo",
                    args: &["test"],
                }),
                Some(system::System::Make) => Some(Cmd::Cmd {
                    bin: "make",
                    args: &["test"],
                }),
                None => None,
            };
        }
    }

    fn infer_run(&mut self, build_system: Option<system::System>) {
        if self.run.is_none() {
            self.run = match build_system {
                Some(system::System::Cabal) => Some(Cmd::Cmd {
                    bin: "cabal",
                    args: &["run"],
                }),
                Some(system::System::Cargo) => Some(Cmd::Cmd {
                    bin: "cargo",
                    args: &["run"],
                }),
                Some(system::System::Make) | None => None,
            };
        }
    }

    fn infer_watch(&mut self, build_system: Option<system::System>) {
        if self.watch.is_none() {
            self.watch = match build_system {
                Some(system::System::Cabal) => Some(Cmd::Cmd {
                    bin: "ghcid",
                    args: &[],
                }),
                _ => {
                    if let (Some(format_cmd), Some(lint_cmd)) = (&self.format, &self.lint) {
                        let fmt_str = format_cmd.to_command_line();
                        let lint_str = lint_cmd.to_command_line();
                        Some(Cmd::Shell(format!(
                            "git ls-files --exclude-standard | entr -c -s '{fmt_str} && {lint_str}'"
                        )))
                    } else if let Some(format_cmd) = &self.format {
                        let fmt_str = format_cmd.to_command_line();
                        Some(Cmd::Shell(format!(
                            "git ls-files --exclude-standard | entr -c -s '{fmt_str}'"
                        )))
                    } else if let Some(lint_cmd) = &self.lint {
                        let lint_str = lint_cmd.to_command_line();
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
    }),
    run: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "exe:crucible-llvm", "--"],
    }),
    watch: Some(Cmd::Cmd {
        bin: "ghcid",
        args: &[],
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
        bin: "hlint",
        args: &[
            "grease-aarch32/src",
            "grease-ppc/src",
            "grease-x86/src",
            "grease-cli/src",
            "grease-exe/main",
            "grease-exe/src",
            "grease-exe/tests",
        ],
    }),
    format: None, // can be guessed from fourmolu.yml
    fix: None,
    build: None, // can be guessed from `.cabal`
    test: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "test:grease-tests", "--"],
    }),
    run: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "exe:grease", "--"],
    }),
    watch: Some(Cmd::Cmd {
        bin: "ghcid",
        args: &[
            "--command",
            "cabal repl lib:grease pkg:grease-cli pkg:grease-exe test:grease-tests",
        ],
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
    }),
    format: None,
    fix: None,
    build: None,
    test: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "test:screach-test", "--"],
    }),
    run: Some(Cmd::Cmd {
        bin: "cabal",
        args: &["run", "exe:screach", "--"],
    }),
    watch: Some(Cmd::Cmd {
        bin: "ghcid",
        args: &["--command", "cabal repl lib:screach exe:screach"],
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
        expansions.push(("l", cmd.to_command_line()));
    }
    if let Some(cmd) = &project.format {
        expansions.push(("f", cmd.to_command_line()));
    }
    if let Some(cmd) = &project.fix {
        expansions.push(("fix", cmd.to_command_line()));
    }
    if let Some(fix) = &project.fix
        && let Some(fmt) = &project.format
    {
        expansions.push((
            "ff",
            format!("{} && {}", fmt.to_command_line(), fix.to_command_line()),
        ));
    }
    if let Some(cmd) = &project.build {
        expansions.push(("b", cmd.to_command_line()));
    }
    if let Some(cmd) = &project.test {
        expansions.push(("t", cmd.to_command_line()));
    }
    if let Some(cmd) = &project.run {
        expansions.push(("r", cmd.to_command_line()));
    }
    if let Some(cmd) = &project.watch {
        expansions.push(("w", cmd.to_command_line()));
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
