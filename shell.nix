with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        clang_11
        cppcheck
        linuxPackages.perf
        python3
        shellcheck
        valgrind
    ];
    shellHook = ''
        . .shellhook
    '';
}
