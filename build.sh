#!/bin/sh

set -e

target_repo="`pwd`/build"
export CHICKEN_REPOSITORY_PATH="$target_repo:$CHICKEN_REPOSITORY_PATH:`chicken-install -repository`"
export CHICKEN_INSTALL_REPOSITORY="$target_repo"

mkdir -p "$target_repo"

for i in lib/*; do
    echo "Building $i"
    (cd "$i"; chicken-install)
done

for i in stb-image srfi-1 srfi-18 srfi-71; do
    echo "Building $i"
    chicken-install "$i"
done

csc -O2 -static -m main main.scm -L "`pkg-config --libs glfw3 portaudio-2.0 opusfile`" -L -lGLESv2
