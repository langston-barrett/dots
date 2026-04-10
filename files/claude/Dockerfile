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

# Install Claude
RUN npm install -g @anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}


# Copy and set up firewall script
# NOTE: build context must be the dots repo root:
#   docker build -f files/claude/Dockerfile -t claude .
COPY files/claude/init-firewall.sh /usr/local/bin/
USER root
RUN chmod +x /usr/local/bin/init-firewall.sh && \
  echo "node ALL=(root) NOPASSWD: /usr/local/bin/init-firewall.sh" > /etc/sudoers.d/node-firewall && \
  chmod 0440 /etc/sudoers.d/node-firewall

USER node

# Install rustup and cargo as node user
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

COPY --chown=node:node kludge/src /tmp/kludge/src
COPY --chown=node:node kludge/Cargo.toml kludge/Cargo.lock* /tmp/kludge/

RUN . ~/.cargo/env && \
    git clone https://github.com/langston-barrett/lun && \
    cd lun && \
    cargo install --path=. && \
    cd .. && \
    rm -rf lun && \
    cargo install mdlynx && \
    cargo install ttlint && \
    cd /tmp/kludge && \
    cargo install --path=. && \
    rm -rf /tmp/kludge

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

# Install Gradle
ARG GRADLE_VERSION=8.13
RUN wget "https://services.gradle.org/distributions/gradle-${GRADLE_VERSION}-bin.zip" -P /tmp && \
    unzip -d /home/node/.local/share /tmp/gradle-${GRADLE_VERSION}-bin.zip && \
    rm /tmp/gradle-${GRADLE_VERSION}-bin.zip

ENV PATH=/home/node/.cargo/bin:/home/node/.local/share/coursier/bin:/home/node/.local/share/gradle-${GRADLE_VERSION}/bin:$PATH
