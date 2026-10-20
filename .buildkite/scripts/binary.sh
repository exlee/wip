#!/usr/bin/env bash
# Builds and tests wip.
# Installs GHC $GHC_VERSION and cabal $CABAL_VERSION through ghcup when the agent lacks them.
# SDL3, SDL3_ttf, hidapi and turbojpeg are loaded at runtime, so the build needs none of them.
# Usage: binary.sh <linux|macos>
# Result: dist/wip-<os>-<arch>
set -euo pipefail

os="$1"
arch="$(uname -m)"

# GHC needs a C toolchain and the gmp, ffi, ncurses and zlib headers.
install_linux_packages() {
  if dpkg -s libgmp-dev libffi-dev zlib1g-dev >/dev/null 2>&1 || ! command -v apt-get >/dev/null; then
    return 0
  fi
  echo "--- :ubuntu: Installing build packages"
  local sudo=""
  [[ "$(id -u)" -ne 0 ]] && sudo="sudo -n"
  $sudo apt-get update -qq
  $sudo apt-get install -y -qq build-essential curl git libffi-dev libgmp-dev libncurses-dev zlib1g-dev pkg-config
}

[[ "$os" == linux ]] && install_linux_packages

# Use the SDK of the selected Xcode, so the linker and the SDK come from the same release.
# Without it, a newer Command Line Tools SDK can be picked up, and its .tbd files fail with
# "tapi error: malformed file ... unknown architecture".
if [[ "$os" == macos ]]; then
  SDKROOT="$(xcrun --show-sdk-path)"
  export SDKROOT
  echo "--- :mac: Toolchain"
  echo "developer dir: $(xcode-select -p)"
  echo "SDKROOT: ${SDKROOT}"
  # sed reads all of ld output; head would exit early and fail the step with SIGPIPE (status 141).
  ld -v 2>&1 | sed -n 1p
fi

# GHC, cabal and the cabal store live in .ci-cache, the hosted agent cache volume of this step.
cache="${PWD}/.ci-cache"
mkdir -p "$cache"
export GHCUP_INSTALL_BASE_PREFIX="$cache"
export CABAL_DIR="${cache}/cabal"
export PATH="${cache}/.ghcup/bin:${PATH}"
echo "--- :floppy_disk: Cache ${cache}"
du -sh "$cache" 2>/dev/null || true

# Installed GHC refers to its install path; a cache restored at another path is reinstalled.
if command -v ghc >/dev/null && ! ghc --numeric-version >/dev/null 2>&1; then
  echo "Cached GHC does not run here, reinstalling"
  rm -rf "${cache}/.ghcup" "${CABAL_DIR}/store"
fi
if ! command -v ghcup >/dev/null; then
  echo "--- :haskell: Installing ghcup"
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org |
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
fi

echo "--- :haskell: GHC ${GHC_VERSION}, cabal ${CABAL_VERSION}"
ghcup install ghc "$GHC_VERSION" --set
ghcup install cabal "$CABAL_VERSION" --set
ghc --version
cabal --version

echo "--- :haskell: Dependencies"
cabal update
cabal build --only-dependencies --enable-tests all

echo "+++ :haskell: Build and test"
cabal build --enable-tests all
cabal test pkg:wip

echo "+++ :ruby: Skill scripts"
if command -v ruby >/dev/null; then
  ruby skills/test/skills_test.rb
else
  echo "ruby not installed, skipped"
fi

out="dist/wip-${os}-${arch}"
mkdir -p dist
cp "$(cabal list-bin exe:wip)" "$out"
chmod +x "$out"
# strip on macOS breaks the linker's ad-hoc signature, and arm64 macOS kills unsigned binaries.
[[ "$os" == linux ]] && strip "$out"
"$out" --help >/dev/null
ls -l "$out"
