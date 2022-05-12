unset PATH
for p in $baseInputs $buildInputs; do
  export PATH=$p/bin${PATH:+:}$PATH
done

function buildPhase() {
    export LOCALE_ARCHIVE_2_27="${locales}/lib/locale/locale-archive"
    mkdir test
    ln -s $compile_queue_test test/compile-queue-test.el
    ln -s $compile_queue_dsl_test test/compile-queue-dsl-test.el
    ln -s $compile_queue compile-queue.el
    ln -s $compile_queue_utils compile-queue-utils.el
    ln -s $compile_queue_structs compile-queue-structs.el
    ln -s $compile_queue_dsl compile-queue-dsl.el
    ln -s $compile_queue_view_mode compile-queue-view-mode.el
    ln -s $compile_queue_delegate_mode compile-queue-delegate-mode.el
    ln -s $compile_queue_org compile-queue-org.el
    ln -s $compile_queue_shell_command compile-queue-shell-command.el
    ln -s $ert_config .ert-runner

    ${emacs}/bin/emacs -q -version
    ${emacs}/bin/emacs -q -batch -L . -l compile-queue.el -l ert-runner | tee $out
    STATUS="${PIPESTATUS[0]}"
    if [ $STATUS -gt 0 ]
    then
        rm $out
        exit $STATUS
    fi
}

function genericBuild() {
  buildPhase
}
