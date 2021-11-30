#!/bin/bash

COQ_DIR="/Users/adampetz/Documents/Fall_2020/copland-avm/src"
HS_DIR="copland-interp/extracted_src/"

cp ${COQ_DIR}/Datatypes.hs ${HS_DIR}
cp ${COQ_DIR}/Term_Defs.hs ${HS_DIR}
cp ${COQ_DIR}/ConcreteEvidence.hs ${HS_DIR}
#cp ${COQ_DIR}/Axioms_Io.hs copland-interp/src/
cp ${COQ_DIR}/Evidence_Bundlers.hs ${HS_DIR}
cp ${COQ_DIR}/StVM.hs ${HS_DIR}
cp ${COQ_DIR}/MonadVM.hs ${HS_DIR}
cp ${COQ_DIR}/Impl_VM.hs ${HS_DIR}

