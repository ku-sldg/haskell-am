#!/bin/bash

COQ_DIR="/Users/adampetz/Documents/Spring_2022/copland-avm/src"
HS_DIR="../copland-interp/extracted_src/"

cp ${COQ_DIR}/Datatypes.hs ${HS_DIR}
cp ${COQ_DIR}/Term_Defs.hs ${HS_DIR}
cp ${COQ_DIR}/ConcreteEvidence.hs ${HS_DIR}
#cp ${COQ_DIR}/Axioms_Io.hs copland-interp/src/
cp ${COQ_DIR}/Evidence_Bundlers.hs ${HS_DIR}
cp ${COQ_DIR}/StVM.hs ${HS_DIR}
cp ${COQ_DIR}/MonadVM.hs ${HS_DIR}
cp ${COQ_DIR}/Impl_VM.hs ${HS_DIR}
#
cp ${COQ_DIR}/Impl_appraisal_alt.hs ${HS_DIR}
cp ${COQ_DIR}/List.hs ${HS_DIR}
cp ${COQ_DIR}/Appraisal_Defs.hs ${HS_DIR}
cp ${COQ_DIR}/Appraisal_Evidence.hs ${HS_DIR}

cp ${COQ_DIR}/Example_Phrases.hs ${HS_DIR}

