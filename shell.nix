with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        clang_10
        cppcheck
        ghc
        hlint
        ormolu
        python38
        shellcheck
        valgrind
    ];
    shellHook = ''
        . .shellhook
    '';
}
