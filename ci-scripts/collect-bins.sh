#!/bin/bash

set -euxo pipefail
declare -x ARTIFACTS_DIR
declare -x ARTIFACTS_TARBALL

if type -P cabal-plan; then
    CABAL_PLAN=cabal-plan
else
    curl --location "https://github.com/haskell-hvr/cabal-plan/releases/download/v0.6.2.0/cabal-plan-0.6.2.0-x86_64-linux.xz" -o cabal-plan.xz
    unxz cabal-plan.xz
    chmod +x cabal-plan
    CABAL_PLAN=$(readlink -f ./cabal-plan)
fi


TEST_DIR="${ARTIFACTS_DIR}/tests"
TEST_LIST="${ARTIFACTS_DIR}/tests.list"
BIN_DIR="${ARTIFACTS_DIR}/bin"
mkdir -p "${TEST_DIR}" "${BIN_DIR}"

"${CABAL_PLAN}" --hide-builtin --hide-global list-bins '*:test:*' | while read -r SEP; do
    TARG=$(echo "${SEP}" | gawk '{ print $2 }')
    if [[ -f "${TARG}" ]]; then
        COMP=$(echo "${SEP}" | gawk '{ print $1 }')
        PACKAGE=$(echo "${COMP}" | cut -d: -f1)
        TEST=$(echo "${COMP}" | cut -d: -f3)
        REL_PATH="tests/${PACKAGE}/${TEST}"
        DEST="${ARTIFACTS_DIR}/${REL_PATH}"
        mkdir -p "$(dirname "${DEST}")"
        cp "${TARG}" "${DEST}"
        echo "${COMP} ${REL_PATH}" >> "${TEST_LIST}"
    fi
done

set +o pipefail
"${CABAL_PLAN}" --hide-builtin --hide-global list-bins '*:exe:*' | grep "$(pwd)"  \
| gawk '{ print $2 }' | while read -r TARG; do
    if [[ -f "${TARG}" ]]; then
        cp "${TARG}" "${BIN_DIR}"
    fi
done

tar caf "${ARTIFACTS_TARBALL}" "${ARTIFACTS_DIR}"
