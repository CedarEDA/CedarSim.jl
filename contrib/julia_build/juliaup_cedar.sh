#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

if [[ -z "$(which juliaup 2>/dev/null)" ]]; then
    echo "Must install juliaup first!  Run the following:" >&2
    echo >&2
    echo "    curl -L https://install.julialang.org | sh" >&2
    exit 1
fi

FLAVOR="${1:-vanilla}"
GITSHA="${2:-latest}"

stat_mtime() {
    if [[ "${OS}" == "macos" ]]; then
        stat -f "%m" "$@" 2>/dev/null || true
    else
        stat -c "%Y" "$@" 2>/dev/null || true
    fi
}

STORAGE_DIR="${HOME}/dist/julia-cedar"
echo "Storing julia cedar build in ${STORAGE_DIR}"

OS="$(if [ "$(uname -s)" = Darwin ]; then echo "macos"; else echo "linux"; fi)"
ARCH="$(uname -m)"
if [ "${ARCH}" = "arm64" ]; then
    ARCH=aarch64
fi

JULIA_BUILD_URL="https://jc-cedarsim-juliabuilds.s3.amazonaws.com/${FLAVOR}/julia-${GITSHA}-${OS}-${ARCH}.tar.gz"
echo "Downloading from ${JULIA_BUILD_URL}"

ARCHIVE_PATH="${STORAGE_DIR}/downloads/$(basename "${JULIA_BUILD_URL}")"
mkdir -p "${STORAGE_DIR}/downloads"

# Download the tarball, but only if upstream has changed since the last time we downloaded
ORIG_MTIME=$(stat_mtime "${ARCHIVE_PATH}")
curl -s#Lf --show-error "${JULIA_BUILD_URL}" -o "${ARCHIVE_PATH}" -z "${ARCHIVE_PATH}"

UNPACK_DEST="${STORAGE_DIR}/${FLAVOR}/${GITSHA}"
if [[ ! -d "${UNPACK_DEST}/bin" || "${ORIG_MTIME}" != "$(stat_mtime "${ARCHIVE_PATH}")" ]]; then
    echo "New or updated installation detected, unpacking!"
    rm -rf "${UNPACK_DEST}"
    mkdir -p "${UNPACK_DEST}"
    tar -xof "${ARCHIVE_PATH}" --strip-components=1 -C "${UNPACK_DEST}"

    if [[ "${OS}" == "macos" ]]; then
        "${SCRIPT_DIR}/codesign.sh" "${UNPACK_DEST}"
    fi
fi

echo "Linking to juliaup as '+cedar-${FLAVOR}-${GITSHA}':"
juliaup remove "cedar-${FLAVOR}-${GITSHA}" || true
juliaup link "cedar-${FLAVOR}-${GITSHA}" "${UNPACK_DEST}/bin/julia"

if [[ "${GITSHA}" == "latest" ]]; then
    echo " -> Also as '+cedar-${FLAVOR}':"
    juliaup remove "cedar-${FLAVOR}" || true
    juliaup link "cedar-${FLAVOR}" "${UNPACK_DEST}/bin/julia"

    if [[ "${FLAVOR}" == "vanilla" ]]; then
        echo " -> Also as '+cedar':"
        juliaup remove "cedar" || true
        juliaup link "cedar" "${UNPACK_DEST}/bin/julia"
    fi
fi

echo "Installation successful!"
julia "+cedar-${FLAVOR}-${GITSHA}" --startup=no -e 'using InteractiveUtils; versioninfo()'
