with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        clang_10
        cppcheck
        python38
        shellcheck
        valgrind
    ];
    shellHook = ''
        . .shellhook
    '';
}
