#!/bin/bash

set -euo pipefail
NUM_TESTS=0
NUM_FAILS=0
declare -a failed
failed=()
while read -r LINE; do
    echo "----------"
    echo
    NUM_TESTS=$((NUM_TESTS + 1))
    COMP=$(echo "${LINE}" | gawk '{ print $1 }')
    TARGET=$(echo "${LINE}" | gawk '{ print $2 }')
    echo "Running: ${COMP}"
    set +e
    if "./artifacts/${TARGET}"; then
        set -e
        echo "${COMP}: Success"
    else
        set -e
        echo "${COMP}: FAILED"
        NUM_FAILS=$((NUM_FAILS + 1))
        failed+=("${COMP}")
    fi
    echo
done < ./artifacts/tests.list

set -e
echo "----------"
echo

if [[ ${NUM_FAILS} -gt 0 ]]; then
    echo "${NUM_FAILS} out of ${NUM_TESTS} test-suites FAILED!"
    echo "FAILED: ${failed[*]}"
    exit 1
else
    echo "All ${NUM_TESTS} test-suites passed."
fi