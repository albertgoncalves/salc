with import <nixpkgs> {};
mkShell {
    buildInputs = [
        ghc
        hlint
        ormolu
        python38
        shellcheck
    ];
    shellHook = ''
        . .shellhook
    '';
}
