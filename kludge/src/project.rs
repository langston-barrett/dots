use std::env;

#[derive(Debug)]
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
    lint: Some(("./scripts/lint/lint.py", &[])),
    format: Some(("./scripts/lint/lint.py", &["--format"])),
    build: None,
    test: None,
    run: None,
    watch: None,
    aliases: &[(
        "w",
        "git ls-files --exclude-standard | entr -c -s './scripts/lint/lint.py --format && ./scripts/lint/lint.py'",
    )],
};

const KLUDGE: Project = Project {
    name: "kludge",
    lint: None,
    format: None,
    build: None,
    test: None,
    run: None,
    watch: None,
    aliases: &[(
        "l",
        "cargo fmt --check && cargo clippy --all-targets -- --deny warnings",
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

const GREASE_CLI: Project = Project {
    name: "grease-cli",
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
    format: None,
    build: None,
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

const GREASE_EXE: Project = Project {
    name: "grease-exe",
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
    format: None,
    build: None,
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

pub(crate) const PROJECTS: &[Project] = &[
    CRUCIBLE_LLVM_CLI,
    DETECT,
    DOTS,
    KLUDGE,
    GREASE,
    GREASE_CLI,
    GREASE_EXE,
    SCREACH,
];

pub(crate) fn project() -> Option<&'static Project> {
    env::current_dir().ok().and_then(|dir| {
        dir.file_name()
            .and_then(|n| n.to_str())
            .and_then(|name| PROJECTS.iter().find(|p| p.name == name))
    })
}
