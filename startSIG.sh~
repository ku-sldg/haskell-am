#! /bin/bash

##  I found this solution to reliably determine the directory
##  in which this script resides at:
##  https://www.ostricher.com/2014/10/the-right-way-to-get-the-directory-of-a-bash-script/

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export COPLAND_BUILD=${SCRIPT_DIR}

cd ${SCRIPT_DIR}/copland-interp
stack exec connection-server-exe &
