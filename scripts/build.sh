#!/usr/bin/env bash

set -euo pipefail

if [ ! -d "$WD/bin" ]; then
    mkdir "$WD/bin"
fi

now () {
    date +%s.%N
}

flags=(
    -fdiagnostics-color=always
    -funbox-strict-fields
    -Wall
    -Wcompat
    -Werror
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmonomorphism-restriction
    -Wpartial-fields
    -Wredundant-constraints
    -Wunused-packages
    -Wunused-type-patterns
)

(
    start=$(now)
    (
        cd "$WD/src"
        hlint ./*.hs
        ormolu -i ./*.hs
        ghc "${flags[@]}" -o "$WD/bin/test" -outputdir "$WD/build/test" Test.hs
        ghc "${flags[@]}" -o "$WD/bin/main" -outputdir "$WD/build/main" Main.hs
    )
    end=$(now)
    python3 -c "print(\"Compiled! ({:.3f}s)\".format(${end} - ${start}))"
)

"$WD/bin/test"
