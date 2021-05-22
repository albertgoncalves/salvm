with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        clang_11
        cppcheck
        python3
        shellcheck
        valgrind
    ];
    shellHook = ''
        . .shellhook
    '';
}
