FROM node:25-trixie

ARG TZ
ENV TZ="$TZ"

ARG CLAUDE_CODE_VERSION=latest

# Install basic development tools and iptables/ipset
RUN apt-get update && apt-get install -y --no-install-recommends \
  aggregate \
  clang \
  curl \
  dnsutils \
  fzf \
  gh \
  git \
  gnupg2 \
  iproute2 \
  ipset \
  iptables \
  jq \
  just \
  less \
  linux-perf \
  man-db \
  procps \
  racket \
  sudo \
  unzip \
  valgrind \
  wget \
  shellcheck \
  vim \
  zsh \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /opt
RUN wget https://musl.cc/arm-linux-musleabi-cross.tgz
RUN tar xzf arm-linux-musleabi-cross.tgz
RUN ln -s /opt/arm-linux-musleabi-cross/bin/arm-linux-musleabi-gcc /usr/local/bin/armv7l-linux-musleabi-gcc
RUN ln -s /opt/arm-linux-musleabi-cross/bin/arm-linux-musleabi-strip /usr/local/bin/armv7l-linux-musleabi-strip
ENV PATH="/opt/arm-linux-musleabi-cross/bin:$PATH"
RUN wget https://musl.cc/powerpc-linux-musl-cross.tgz
RUN tar xzf powerpc-linux-musl-cross.tgz
ENV PATH="/opt/powerpc-linux-musl-cross/bin:$PATH"

# ghc
RUN apt update && \
    apt install -y \
      # ghcup requirements \
      build-essential libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6 \
      # Although ghcup's version of GHC shouldn't require libnuma, the aarch64 \
      # version does impose this requirement for unknown reasons. \
      # (See https://gitlab.haskell.org/ghc/ghc/-/issues/20876#note_399802) \
      libnuma1 \
      # GREASE dependencies \
      pkg-config zlib1g-dev \
      # Miscellaneous \
      git locales unzip wget && \
    rm -rf /var/lib/apt/lists/*

ARG TARGETPLATFORM
# If you update the values below, make sure to also update them in
# .github/workflows/ci.yaml.
ENV WHAT4_SOLVERS_VER=snapshot-20251112
ENV GHCUP_VER=0.1.50.2
ENV CABAL_VER=3.14.2.0
ENV GHC_VER=9.10.2

WORKDIR /usr/local/bin
RUN case ${TARGETPLATFORM} in \
      "linux/amd64") \
        WHAT4_SOLVERS_ARCH=X64 ;; \
      "linux/arm64" | "linux/arm64/v8") \
        WHAT4_SOLVERS_ARCH=ARM64 ;; \
      *) \
        printf "Unsupported architecture: %s\n" "${TARGETPLATFORM}" >&2; \
        exit 1 ;; \
    esac && \
    curl -o solvers.zip -sL "https://github.com/GaloisInc/what4-solvers/releases/download/${WHAT4_SOLVERS_VER}/ubuntu-24.04-${WHAT4_SOLVERS_ARCH}-bin.zip" && \
    unzip solvers.zip && \
    rm solvers.zip && \
    chmod +x *

# Support Unicode via UTF-8
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Ensure default node user has access to /usr/local/share
RUN mkdir -p /usr/local/share/npm-global && \
  chown -R node:node /usr/local/share

RUN apt-get update && apt-get install -y --no-install-recommends \
  llvm-19-dev llvm-19-tools cmake ninja-build python3-matplotlib \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

ARG USERNAME=node

ENV HOME=/home/node
ENV PATH=/home/node/ghcup-download/bin:/home/node/.ghcup/bin:$PATH
RUN case ${TARGETPLATFORM} in \
      "linux/amd64") \
        GHCUP_ARCH=x86_64 ;; \
      "linux/arm64" | "linux/arm64/v8") \
        GHCUP_ARCH=aarch64 ;; \
      *) \
        printf "Unsupported architecture: %s\n" "${TARGETPLATFORM}" >&2; \
        exit 1 ;; \
    esac && \
    mkdir -p /home/node/ghcup-download/bin && \
    curl -L "https://downloads.haskell.org/~ghcup/${GHCUP_VER}/${GHCUP_ARCH}-linux-ghcup-${GHCUP_VER}" -o /home/node/ghcup-download/bin/ghcup && \
    chmod +x /home/node/ghcup-download/bin/ghcup && \
    mkdir -p /home/node/.ghcup && \
    ghcup --version && \
    ghcup install cabal ${CABAL_VER} && \
    ghcup install ghc ${GHC_VER} && \
    ghcup set ghc ${GHC_VER}
RUN cabal update && \
    cabal install happy --install-method=copy --installdir=/usr/local/bin && \
    cabal install alex-3.2.7.4 --install-method=copy --installdir=/usr/local/bin
RUN cabal v2-update && chown -R node:node /home/node

# Persist bash history.
RUN SNIPPET="export PROMPT_COMMAND='history -a' && export HISTFILE=/commandhistory/.bash_history" \
  && mkdir /commandhistory \
  && touch /commandhistory/.bash_history \
  && chown -R $USERNAME /commandhistory

# Set `DEVCONTAINER` environment variable to help with orientation
ENV DEVCONTAINER=true

# Create workspace and config directories and set permissions
RUN mkdir -p /workspace /home/node/.claude && \
  chown -R node:node /workspace /home/node/.claude

WORKDIR /workspace

ARG GIT_DELTA_VERSION=0.18.2
RUN ARCH=$(dpkg --print-architecture) && \
  wget "https://github.com/dandavison/delta/releases/download/${GIT_DELTA_VERSION}/git-delta_${GIT_DELTA_VERSION}_${ARCH}.deb" && \
  sudo dpkg -i "git-delta_${GIT_DELTA_VERSION}_${ARCH}.deb" && \
  rm "git-delta_${GIT_DELTA_VERSION}_${ARCH}.deb"

# Set up non-root user
USER node

# Install global packages
ENV NPM_CONFIG_PREFIX=/usr/local/share/npm-global
ENV PATH=$PATH:/usr/local/share/npm-global/bin

# Set the default shell to zsh rather than sh
ENV SHELL=/bin/zsh

# Set the default editor and visual
ENV EDITOR=nano
ENV VISUAL=nano

# Default powerline10k theme
ARG ZSH_IN_DOCKER_VERSION=1.2.0
RUN sh -c "$(wget -O- https://github.com/deluan/zsh-in-docker/releases/download/v${ZSH_IN_DOCKER_VERSION}/zsh-in-docker.sh)" -- \
  -p git \
  -p fzf \
  -a "source /usr/share/doc/fzf/examples/key-bindings.zsh" \
  -a "source /usr/share/doc/fzf/examples/completion.zsh" \
  -a "export PROMPT_COMMAND='history -a' && export HISTFILE=/commandhistory/.bash_history" \
  -x

# Install Claude/Codex
RUN npm install -g @anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}

# Install Nix (single-user mode for the node user)
# USER root
# RUN mkdir -m 0755 /nix && chown node:node /nix
# USER node
# RUN curl -L https://nixos.org/nix/install | sh -s -- --no-daemon
# ENV PATH=/home/node/.nix-profile/bin:$PATH

# Install Pi coding agent
RUN npm install -g --ignore-scripts @earendil-works/pi-coding-agent

RUN curl -fsSL https://chatgpt.com/codex/install.sh | CODEX_NON_INTERACTIVE=1 sh
RUN echo 'model_provider = "amazon-bedrock"' >> ~/.codex/config.toml

# Copy and set up firewall script
# NOTE: build context must be the dots repo root:
#   docker build -f files/claude/Dockerfile -t claude .
COPY files/claude/init-firewall.sh /usr/local/bin/
USER root
RUN chmod +x /usr/local/bin/init-firewall.sh && \
  echo "node ALL=(root) NOPASSWD: /usr/local/bin/init-firewall.sh" > /etc/sudoers.d/node-firewall && \
  chmod 0440 /etc/sudoers.d/node-firewall

USER node

RUN curl -LsSf https://astral.sh/uv/0.11.7/install.sh | sh

# Install Nix
USER root
RUN mkdir -m 0755 /nix && chown node /nix
USER node
RUN mkdir -p /home/node/.config/nix && \
    echo "sandbox = false" > /home/node/.config/nix/nix.conf && \
    echo "filter-syscalls = false" >> /home/node/.config/nix/nix.conf && \
    curl -L https://nixos.org/nix/install | sh -s -- --no-daemon --no-channel-add && \
    echo 'if [ -e /home/node/.nix-profile/etc/profile.d/nix.sh ]; then . /home/node/.nix-profile/etc/profile.d/nix.sh; fi' >> ~/.zshrc && \
    . /home/node/.nix-profile/etc/profile.d/nix.sh && \
    export PATH=/home/node/.nix-profile/bin:$PATH && \
    nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs && \
    nix-channel --update

ENV PATH=/home/node/.nix-profile/bin:$PATH

# Install Java (required for Gradle)
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
  openjdk-21-jdk \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

# Set JAVA_HOME based on architecture
RUN case ${TARGETPLATFORM} in \
      "linux/amd64") \
        echo 'export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64' >> /home/node/.zshrc && \
        echo 'export PATH=$JAVA_HOME/bin:$PATH' >> /home/node/.zshrc ;; \
      "linux/arm64" | "linux/arm64/v8") \
        echo 'export JAVA_HOME=/usr/lib/jvm/java-21-openjdk-arm64' >> /home/node/.zshrc && \
        echo 'export PATH=$JAVA_HOME/bin:$PATH' >> /home/node/.zshrc ;; \
      *) \
        printf "Unsupported architecture: %s\n" "${TARGETPLATFORM}" >&2; \
        exit 1 ;; \
    esac

USER node

ENV JAVA_HOME=/usr/lib/jvm/java-21-openjdk-amd64
ENV PATH=$JAVA_HOME/bin:$PATH

# Install Gradle
ARG GRADLE_VERSION=8.13
RUN wget "https://services.gradle.org/distributions/gradle-${GRADLE_VERSION}-bin.zip" -P /tmp && \
    unzip -d /home/node/.local/share /tmp/gradle-${GRADLE_VERSION}-bin.zip && \
    rm /tmp/gradle-${GRADLE_VERSION}-bin.zip

# Install coursier and spotless for Scala
RUN mkdir -p /home/node/.local/bin && \
    case ${TARGETPLATFORM} in \
      "linux/amd64") \
        CS_ARCH=x86_64 ;; \
      "linux/arm64" | "linux/arm64/v8") \
        CS_ARCH=aarch64 ;; \
      *) \
        printf "Unsupported architecture: %s\n" "${TARGETPLATFORM}" >&2; \
        exit 1 ;; \
    esac && \
    curl -fL "https://github.com/coursier/launchers/raw/master/cs-${CS_ARCH}-pc-linux.gz" | gzip -d > /home/node/.local/bin/cs && \
    chmod +x /home/node/.local/bin/cs && \
    /home/node/.local/bin/cs install scalafmt && \
    /home/node/.local/bin/cs install scala-cli

# Install rustup and cargo as node user
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

ARG RUFF_VERSION=0.15.10
ARG TY_VERSION=0.0.29
ARG TAPLO_VERSION=0.10.0
ARG TTLINT_VERSION=0.1.0
ARG ZIZMOR_VERSION=1.23.1
ARG TYPOS_VERSION=1.45.0
ARG MDLYNX_VERSION=0.3.0
ARG LUN_VERSION=0.13.0
ARG FOURMOLU_VERSION=0.19.0.1
RUN case ${TARGETPLATFORM} in \
      "linux/amd64") ARCH=x86_64 ;; \
      "linux/arm64" | "linux/arm64/v8") ARCH=aarch64 ;; \
      *) printf "Unsupported architecture: %s\n" "${TARGETPLATFORM}" >&2; exit 1 ;; \
    esac && \
    curl -fsSL "https://github.com/astral-sh/ruff/releases/download/${RUFF_VERSION}/ruff-${ARCH}-unknown-linux-musl.tar.gz" \
      | tar -xzf - --strip-components=1 -C /home/node/.cargo/bin && \
    curl -fsSL "https://github.com/astral-sh/ty/releases/download/${TY_VERSION}/ty-${ARCH}-unknown-linux-musl.tar.gz" \
      | tar -xzf - --strip-components=1 -C /home/node/.cargo/bin && \
    curl -fsSL "https://github.com/tamasfe/taplo/releases/download/${TAPLO_VERSION}/taplo-linux-${ARCH}.gz" \
      | gunzip > /home/node/.cargo/bin/taplo && \
    chmod +x /home/node/.cargo/bin/taplo && \
    curl -fsSL "https://github.com/woodruffw/zizmor/releases/download/v${ZIZMOR_VERSION}/zizmor-${ARCH}-unknown-linux-gnu.tar.gz" \
      | tar -xzf - -C /home/node/.cargo/bin ./zizmor && \
    curl -fsSL "https://github.com/crate-ci/typos/releases/download/v${TYPOS_VERSION}/typos-v${TYPOS_VERSION}-${ARCH}-unknown-linux-musl.tar.gz" \
      | tar -xzf - -C /home/node/.cargo/bin ./typos && \
    if [ "${ARCH}" = "x86_64" ]; then \
        curl -fsSL "https://github.com/langston-barrett/ttlint/releases/download/v${TTLINT_VERSION}/ttlint-${ARCH}-unknown-linux-musl.gz" \
          | gunzip > /home/node/.cargo/bin/ttlint && \
        chmod +x /home/node/.cargo/bin/ttlint && \
        curl -fsSL "https://github.com/langston-barrett/mdlynx/releases/download/v${MDLYNX_VERSION}/mdlynx-${ARCH}-unknown-linux-musl" \
          -o /home/node/.cargo/bin/mdlynx && \
        chmod +x /home/node/.cargo/bin/mdlynx && \
        curl -fsSL "https://github.com/langston-barrett/lun/releases/download/v${LUN_VERSION}/lun-${ARCH}-unknown-linux-gnu.gz" \
          | gunzip > /home/node/.cargo/bin/lun && \
        chmod +x /home/node/.cargo/bin/lun && \
        curl -fsSL "https://github.com/fourmolu/fourmolu/releases/download/v${FOURMOLU_VERSION}/fourmolu-${FOURMOLU_VERSION}-linux-x86_64" \
          -o /home/node/.cargo/bin/fourmolu && \
        chmod +x /home/node/.cargo/bin/fourmolu; \
    fi

RUN case ${TARGETPLATFORM} in \
      "linux/arm64" | "linux/arm64/v8") \
        . ~/.cargo/env && \
        cargo install ttlint && \
        cargo install mdlynx && \
        git clone https://github.com/langston-barrett/lun && \
        cd lun && cargo install --path=. && cd .. && rm -rf lun ;; \
    esac

COPY --chown=node:node kludge/src /tmp/kludge/src
COPY --chown=node:node kludge/Cargo.toml kludge/Cargo.lock* /tmp/kludge/

RUN . ~/.cargo/env && \
    cd /tmp/kludge && \
    cargo install --path=. && \
    rm -rf /tmp/kludge

# Install Cryptol (X64 only - ARM64 binaries not available for Ubuntu 24.04)
ARG CRYPTOL_VERSION=3.5.0
RUN case ${TARGETPLATFORM} in \
      "linux/amd64") \
        curl -fsSL "https://github.com/GaloisInc/cryptol/releases/download/${CRYPTOL_VERSION}/cryptol-${CRYPTOL_VERSION}-ubuntu-24.04-X64.tar.gz" \
          | tar -xzf - -C /tmp && \
        cp /tmp/cryptol-${CRYPTOL_VERSION}-ubuntu-24.04-X64/bin/cryptol /home/node/.cargo/bin/ && \
        rm -rf /tmp/cryptol-${CRYPTOL_VERSION}-ubuntu-24.04-X64 ;; \
      "linux/arm64" | "linux/arm64/v8") \
        printf "Cryptol binary not available for ARM64, skipping installation\n" ;; \
      *) \
        printf "Unsupported architecture: %s\n" "${TARGETPLATFORM}" >&2; \
        exit 1 ;; \
    esac

# Install SAW (X64 only - ARM64 Linux binaries not available)
ARG SAW_VERSION=1.5.1
RUN case ${TARGETPLATFORM} in \
      "linux/amd64") \
        curl -fsSL "https://github.com/GaloisInc/saw-script/releases/download/v${SAW_VERSION}/saw-${SAW_VERSION}-ubuntu-24.04-X64.tar.gz" \
          | tar -xzf - -C /tmp && \
        cp /tmp/saw-${SAW_VERSION}-ubuntu-24.04-X64/bin/saw /home/node/.cargo/bin/ && \
        rm -rf /tmp/saw-${SAW_VERSION}-ubuntu-24.04-X64 ;; \
      "linux/arm64" | "linux/arm64/v8") \
        printf "SAW binary not available for ARM64, skipping installation\n" ;; \
      *) \
        printf "Unsupported architecture: %s\n" "${TARGETPLATFORM}" >&2; \
        exit 1 ;; \
    esac

ENV PATH=/home/node/.cargo/bin:/home/node/.local/share/coursier/bin:/home/node/.local/share/gradle-${GRADLE_VERSION}/bin:$PATH
ENV KLUDGE_LUN_SKIP_TOOLS=stale-todo.py
