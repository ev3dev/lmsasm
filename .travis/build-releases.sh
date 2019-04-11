#!/bin/bash

set -e
set -x

build_release() {
    GOOS=$1
    GOARCH=$2
    mkdir -p $RELEASE_DIR/lmsasm-$TRAVIS_TAG-$GOOS-$GOARCH
    cd $RELEASE_DIR/lmsasm-$TRAVIS_TAG-$GOOS-$GOARCH
    go build github.com/ev3dev/lmsasm/lmsasm
    go build github.com/ev3dev/lmsasm/lmsgen
    cp $TRAVIS_BUILD_DIR/LICENSE.txt .
    zip $RELEASE_DIR/lmsasm-$TRAVIS_TAG-$GOOS-$GOARCH.zip *
}

build_release darwin amd64
build_release linux amd64
build_release windows amd64
