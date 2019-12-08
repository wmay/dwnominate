#!/bin/bash

if [$TRAVIS_OS_NAME = 'osx']; then

    # Install custom requirements on macOS

    # recommended clang version depends on the R minor version (see
    # https://cran.r-project.org/bin/macosx/tools/ )
    case "${TRAVIS_R_VERSION:0:3}" in
	3.5) clang_version=6;;
	3.6) clang_version=7;;
	3.7) clang_version=8;;
    esac

    # install clang
    clang_pkg=clang-${clang_version}.0.0.pkg
    clang_path=/tmp/${clang_pkg}
    curl -fLo /tmp/ https://cloud.r-project.org/bin/macosx/tools/${clang_pkg}
    sudo installer -pkg $clang_path -target /
    rm $clang_pkg

    # add clang info to .R/Makevars
    mkdir .R
    echo "CC = /usr/local/clang${clang_version}/bin/clang" >> .R/Makevars
    echo "CXX = /usr/local/clang${clang_version}/bin/clang++" >> .R/Makevars
fi
