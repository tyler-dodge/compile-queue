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
    versioned_emacs pkgs.coreutils pkgs.glibcLocales];
  emacs = versioned_emacs;
  locales = pkgs.glibcLocales;
  compile_queue = ../compile-queue.el;
  compile_queue_utils = ../compile-queue-utils.el;
  compile_queue_structs = ../compile-queue-structs.el;
  compile_queue_dsl = ../compile-queue-dsl.el;
  compile_queue_view_mode = ../compile-queue-view-mode.el;
  compile_queue_delegate_mode = ../compile-queue-delegate-mode.el;
  compile_queue_org = ../compile-queue-org.el;
  compile_queue_shell_command = ../compile-queue-shell-command.el;
  compile_queue_test = ../test/compile-queue-test.el;
  compile_queue_dsl_test = ../test/compile-queue-dsl-test.el;
  ert_config = ../ert_config;
  system = builtins.currentSystem;
}

  
