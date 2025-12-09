use std::{env, path::Path, process::Command};

use crate::system;

#[derive(Clone, Debug)]
pub(crate) struct Project {
    pub(crate) name: &'static str,
    pub(crate) lint: Option<(&'static str, &'static [&'static str])>,
    pub(crate) format: Option<(&'static str, &'static [&'static str])>,
    pub(crate) build: Option<(&'static str, &'static [&'static str])>,
    pub(crate) test: Option<(&'static str, &'static [&'static str])>,
    pub(crate) run: Option<(&'static str, &'static [&'static str])>,
    pub(crate) watch: Option<(&'static str, &'static [&'static str])>,
    pub(crate) aliases: &'static [(&'static str, &'static str)],
}

impl Project {
    pub(crate) fn infer(&mut self) {
        let Ok(pwd) = env::current_dir() else {
            return;
        };

        let build_system = system::System::detect(&pwd);
        let has_lint_py = Path::new("scripts/lint/lint.py").exists();

        if self.lint.is_none() {
            if has_lint_py {
                self.lint = Some(("scripts/lint/lint.py", &[]));
            } else {
                self.lint = match build_system {
                    Some(system::System::Cargo) => Some((
                        "cargo",
                        &["clippy", "--all-targets", "--", "--deny", "warnings"],
                    )),
                    Some(system::System::Make) => Some(("make", &["lint"])),
                    _ => None,
                };
            }
        }

        if self.format.is_none() {
            if has_lint_py {
                self.format = Some(("scripts/lint/lint.py", &["--format"]));
            } else {
                self.format = match build_system {
                    Some(system::System::Cabal) => {
                        Some(("fourmolu", &["--mode", "inplace", "$(git ls-files '*.hs')"]))
                    }
                    Some(system::System::Cargo) => Some(("cargo", &["fmt"])),
                    Some(system::System::Make) => Some(("make", &["fmt"])),
                    None => None,
                };
            }
        }

        if self.build.is_none() {
            self.build = match build_system {
                Some(system::System::Cabal) => Some(("cabal", &["build"])),
                Some(system::System::Cargo) => Some(("cargo", &["build"])),
                Some(system::System::Make) => Some(("make", &[])),
                None => None,
            };
        }

        if self.test.is_none() {
            self.test = match build_system {
                Some(system::System::Cabal) => Some(("cabal", &["test"])),
                Some(system::System::Cargo) => Some(("cargo", &["test"])),
                Some(system::System::Make) => Some(("make", &["test"])),
                None => None,
            };
        }

        if self.run.is_none() {
            self.run = match build_system {
                Some(system::System::Cabal) => Some(("cabal", &["run"])),
                Some(system::System::Cargo) => Some(("cargo", &["run"])),
                Some(system::System::Make) | None => None,
            };
        }

        if self.watch.is_none() {
            self.watch = match build_system {
                Some(system::System::Cabal) => Some(("ghcid", &[])),
                Some(system::System::Cargo) => Some((
                    "bash",
                    &[
                        "-c",
                        "ls ./**/Cargo.toml ./**/*.rs | entr -c -s 'cargo fmt && cargo clippy --all-targets -- --deny warnings'",
                    ],
                )),
                Some(system::System::Make) => Some(("make", &["test"])),
                _ => None,
            };
        }
    }
}

const CRUCIBLE_LLVM_CLI: Project = Project {
    name: "crucible-llvm-cli",
    lint: None,
    format: None,
    build: None,
    test: Some(("cabal", &["run", "test:crucible-llvm-cli-tests", "--"])),
    run: Some(("cabal", &["run", "exe:crucible-llvm", "--"])),
    watch: Some(("ghcid", &[])),
    aliases: &[
        ("rs", "cabal run exe:crucible-llvm -- simulate"),
        ("wt", "ghcid --target=test:crucible-llvm-cli-tests"),
    ],
};

const DETECT: Project = Project {
    name: "detect",
    lint: None,
    format: None,
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

const DOTS: Project = Project {
    name: "dots",
    lint: None,
    format: None,
    build: None,
    test: None,
    run: None,
    watch: None,
    aliases: &[(
        "w",
        "git ls-files --exclude-standard | entr -c -s './scripts/lint/lint.py --format && ./scripts/lint/lint.py'",
    )],
};

const GREASE: Project = Project {
    name: "grease",
    lint: Some((
        "hlint",
        &[
            "grease-aarch32/src",
            "grease-ppc/src",
            "grease-x86/src",
            "grease-cli/src",
            "grease-exe/main",
            "grease-exe/src",
            "grease-exe/tests",
        ],
    )),
    format: None, // can be guessed from fourmolu.yml
    build: None,  // can be guessed from `.cabal`
    test: Some(("cabal", &["run", "test:grease-tests", "--"])),
    run: Some(("cabal", &["run", "exe:grease", "--"])),
    watch: Some((
        "ghcid",
        &[
            "--command",
            "cabal repl lib:grease pkg:grease-cli pkg:grease-exe test:grease-tests",
        ],
    )),
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
    lint: Some((
        "hlint",
        &[
            "--hint=../deps/grease/.hlint.yaml",
            "{app,src,test}",
            "../elf-edit-ecfs/{src,tools}",
        ],
    )),
    format: None,
    build: None,
    test: Some(("cabal", &["run", "test:screach-test", "--"])),
    run: Some(("cabal", &["run", "exe:screach", "--"])),
    watch: Some((
        "ghcid",
        &["--command", "cabal repl lib:screach exe:screach"],
    )),
    aliases: &[("wt", "ghcid --target=test:screach-test")],
};

pub(crate) const PROJECTS: &[Project] = &[CRUCIBLE_LLVM_CLI, DETECT, DOTS, GREASE, SCREACH];

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
        .map(|s| s.to_string())
}

pub(crate) fn project() -> Option<&'static Project> {
    let current = env::current_dir().ok()?;
    let current = current.file_name().and_then(|f| f.to_str())?;
    PROJECTS
        .iter()
        .find(|p| p.name == current)
        .or_else(|| git_root_name().and_then(|name| PROJECTS.iter().find(|p| p.name == name)))
}

fn build_command(cmd: &str, args: &[&str]) -> String {
    if args.is_empty() {
        cmd.to_string()
    } else {
        format!("{} {}", cmd, args.join(" "))
    }
}

pub(crate) fn project_expansions(project: &Project) -> Vec<(&'static str, String)> {
    let mut expansions: Vec<(&str, String)> = Vec::new();

    if let Some((cmd, args)) = &project.lint {
        expansions.push(("l", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = &project.format {
        expansions.push(("f", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = &project.build {
        expansions.push(("b", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = &project.test {
        expansions.push(("t", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = &project.run {
        expansions.push(("r", build_command(cmd, args)));
    }
    if let Some((cmd, args)) = &project.watch {
        expansions.push(("w", build_command(cmd, args)));
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
