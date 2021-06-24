mate-regold() {
  rm -rf llvm/PointerAnalysis/test/gold/*
  export MATE_MAKE_GOLD_TESTS=1
  ./shake.sh -j pytests -- -- -x -k test_pointer_analysis_golden |& tee log
}
