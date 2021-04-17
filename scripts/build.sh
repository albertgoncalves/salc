#!/usr/bin/env bash

set -euo pipefail

for x in bin build; do
    if [ ! -d "$WD/$x" ]; then
        mkdir "$WD/$x"
    fi
done

now () {
    date +%s.%N
}

flags=(
    -fdiagnostics-color=always
    -funbox-strict-fields
    -outputdir "$WD/build"
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
        ghc "${flags[@]}" -o "$WD/bin/test_compiler" Test.hs
    )
    end=$(now)
    python3 -c "print(\"Compiled! ({:.3f}s)\".format(${end} - ${start}))"
)

"$WD/bin/test_compiler"
