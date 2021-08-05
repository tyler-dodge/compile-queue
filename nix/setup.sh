unset PATH
for p in $baseInputs $buildInputs; do
  export PATH=$p/bin${PATH:+:}$PATH
done

function buildPhase() {
    mkdir test
    ln -s $test_target test/compile-queue-test.el
    ${emacs}/bin/emacs -q --version
    ${emacs}/bin/emacs -q -batch -l $compile_queue -l ert-runner
    mkdir $out
}

function genericBuild() {
  buildPhase
}
