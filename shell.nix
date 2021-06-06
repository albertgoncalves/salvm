with import <nixpkgs> {};
mkShell.override { stdenv = llvmPackages_12.stdenv; } {
    buildInputs = [
        linuxPackages.perf
        lld_12
        python3
        shellcheck
        valgrind
    ];
    shellHook = ''
        . .shellhook
    '';
}
