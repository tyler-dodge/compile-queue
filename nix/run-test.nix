{ emacsWithPackages }:
let
  pkgs = import <nixpkgs> {};
  versioned_emacs = emacsWithPackages (epkgs: with epkgs; [
    ert-async
    el-mock
    ert-runner
    uuid
    s
    deferred
    ht
    dash
  ]);
in derivation rec {
  name = "compile-queue";
  baseInputs = [];
  builder = "${pkgs.bash}/bin/bash";
  args = [ ./builder.sh ];
  setup = ./setup.sh;
  buildInputs = [
    versioned_emacs pkgs.coreutils];
  emacs = versioned_emacs;
  compile_queue = ../compile-queue.el;
  test_target = ../test/compile-queue-test.el;
  system = builtins.currentSystem;
}

  
