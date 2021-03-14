with import <nixpkgs> {};
pkgsMusl.mkShell {
    buildInputs = [
        clang_10
        cppcheck
        linuxPackages.perf
        python38
        shellcheck
        valgrind
    ];
    shellHook = ''
        . .shellhook
    '';
}
