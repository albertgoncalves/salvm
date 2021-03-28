with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        clang_10
        cppcheck
        ghc
        hlint
        linuxPackages.perf
        ocaml
        ocamlPackages.ocp-indent
        ormolu
        python38
        shellcheck
        valgrind
    ];
    shellHook = ''
        . .shellhook
    '';
}
