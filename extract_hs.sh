#!/bin/bash

COQ_DIR="/Users/adampetz/Documents/Fall_2020/copland-avm/src"

cp ${COQ_DIR}/Datatypes.hs copland-interp/src/
cp ${COQ_DIR}/Term_Defs.hs copland-interp/src/
cp ${COQ_DIR}/ConcreteEvidence.hs copland-interp/src/
#cp ${COQ_DIR}/Axioms_Io.hs copland-interp/src/
cp ${COQ_DIR}/Evidence_Bundlers.hs copland-interp/src/
cp ${COQ_DIR}/StVM.hs copland-interp/src/
cp ${COQ_DIR}/MonadVM.hs copland-interp/src/
cp ${COQ_DIR}/Impl_VM.hs copland-interp/src/

