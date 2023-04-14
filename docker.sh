#!/bin/bash

set -o nounset

readonly IMAGE=mini-ruccola-ocaml:2

build() {
  docker build \
    --build-arg USER=$USER \
    --build-arg GROUP=$(id -gn) \
    -t $IMAGE .
}

run() {
  docker run --rm -it \
    -v "$(pwd):/home/${USER}/work" \
    $IMAGE "$@"
}

cmd="$1"; shift
case $cmd in
  build | b* )
    build "$@"
;; run | r* )
     run "$@"
;; * )
     echo "invalid command (${cmd})" >&2
     ;;
esac
