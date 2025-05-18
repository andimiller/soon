let
  pkgs = import <unstable> {};
  stdenv = pkgs.stdenv;
in rec {
  clangEnv = stdenv.mkDerivation rec {
    name = "clang-env";
    shellHook = ''
    alias cls=clear
    '';
    LLVM_BIN = pkgs.clang + "/bin";
    buildInputs = with pkgs; [
      stdenv
      sbt
      openjdk21
      boehmgc
      libunwind
      clang
      zlib
      s2n-tls
      nodejs
      utf8proc
    ];
  };
}
