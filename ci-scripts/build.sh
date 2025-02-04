#!/bin/bash

set -eux

cabal configure --jobs '$ncpus' \
    --enable-tests --constraint="lukko -ofd-locking" \
    --extra-lib-dirs=/usr/local/lib
cabal v2-update
git config --global --add safe.directory "${GITHUB_WORKSPACE}"
cabal v2-build all
