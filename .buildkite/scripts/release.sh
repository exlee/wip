#!/usr/bin/env bash
# Publishes the binaries of this build as the GitHub release of $BUILDKITE_TAG.
# Needs the Buildkite secret WIP_GITHUB_TOKEN (fine-grained token, "Contents: read and write").
set -euo pipefail

tag="${BUILDKITE_TAG:?release.sh runs on tag builds only}"
gh_version="2.62.0"
# git@github.com:owner/repo.git or https://github.com/owner/repo(.git) -> owner/repo
repo="$(sed -E 's#^(git@github\.com:|https://github\.com/)##; s#\.git$##' <<<"$BUILDKITE_REPO")"
export GH_REPO="$repo"
GH_TOKEN="$(buildkite-agent secret get WIP_GITHUB_TOKEN)"
export GH_TOKEN

if ! command -v gh >/dev/null; then
  echo "--- :github: Installing gh ${gh_version}"
  tools="${HOME}/.cache/wip-ci"
  mkdir -p "$tools"
  curl -fsSL "https://github.com/cli/cli/releases/download/v${gh_version}/gh_${gh_version}_linux_amd64.tar.gz" |
    tar -xz -C "$tools"
  export PATH="${tools}/gh_${gh_version}_linux_amd64/bin:${PATH}"
fi

echo "--- :package: Downloading binaries"
buildkite-agent artifact download "dist/*" .

# Archives keep the executable bit that a plain download loses.
echo "--- :package: Packaging"
mkdir -p release
for bin in dist/wip-*; do
  name="$(basename "$bin")"
  chmod +x "$bin"
  tar -czf "release/${name}.tar.gz" -C dist "$name"
done
(cd release && sha256sum ./*.tar.gz > SHA256SUMS)
cat release/SHA256SUMS

notes="Built from ${BUILDKITE_COMMIT:0:12} by ${BUILDKITE_BUILD_URL}

Unpack with \`tar -xzf wip-<os>-<arch>.tar.gz\`. Checksums are in SHA256SUMS.
Runtime libraries: SDL3 and SDL3_ttf for the window, hidapi and turbojpeg for the Stream Deck."

echo "+++ :github: Release ${tag}"
if ! gh release view "$tag" >/dev/null 2>&1; then
  gh release create "$tag" --title "$tag" --notes "$notes" --verify-tag
fi
gh release upload "$tag" release/* --clobber
gh release view "$tag" --json url,assets --jq '.url, (.assets[] | "  " + .name)'
