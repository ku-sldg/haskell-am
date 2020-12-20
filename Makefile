SHELL := /bin/bash
export COPLAND_BUILD=${CURDIR}

SERV := copland-interp\:exe\:copland-server-exe
CLIENT := copland-interp\:exe\:copland-client-exe
GEN := copland-interp\:exe\:copland-gen-exe
SIG := copland-interp\:exe\:sig-server-exe
STORE := copland-interp\:exe\:store-server-exe
TESTS := copland-interp\:exe\:test-server-exe
TESTC := copland-interp\:exe\:test-client-exe

all:	./copland-interp/copland-interp.cabal
	cd ./copland-interp/ ; stack build

build_client :	./copland-interp/copland-interp.cabal
		cd ./copland-interp/ ; stack build ${CLIENT}

run:
	cd copland-interp ; stack exec -- copland-client-exe -w -a

run1:
	cd copland-interp ; stack exec -- copland-client-exe -w -n ../names.txt


startCS:
	cd copland-interp ; stack exec connection-server-exe

startSIG:
	cd copland-interp ; stack exec sig-server-exe

runSim:
	cd copland-interp ; stack exec -- copland-client-exe -s -w -v

term:
	cd copland-interp ; stack exec -- copland-client-exe -w -t ../t.hs

termCompiled:
	cd copland-interp ; stack exec -- copland-client-exe -w -compile

termCompiledd:
	cd copland-interp ; stack exec -- copland-client-exe -compile -t ../t.hs

attack:
	./modTarget.sh
repair:
	./repairTarget.sh

provision:
	cd copland-interp ; stack exec -- copland-client-exe -p

runtestserver:
	cd copland-interp ; stack exec -- test-server-exe

runtestclient:
	cd copland-interp ; stack exec -- test-client-exe

#Generate one random(-n 1) Copland term(-t) as a datatype(-d)
gen:
	cd copland-interp ; stack exec -- copland-gen-exe -n 1 -t -d

#Generate one random(-n 1) Copland term(-t) as JSON
genj:
	cd copland-interp ; stack exec -- copland-gen-exe -n 1 -t

#Input Copland terms(-t) as datatypes from stdin, write JSON to stdout
geni:
	cd copland-interp ; stack exec -- copland-gen-exe -t

#Input Copland terms(-t) as JSON from stdin, write terms as datatypes(-d) to stdout
gend:
	cd copland-interp ; stack exec -- copland-gen-exe -t -d

#Input Copland terms(-t) from the file fin.hs(-i ../fin.hs) as datatypes, write JSON to the file fout.txt(-o ../fout.txt)
genf:
	cd copland-interp ; stack exec -- copland-gen-exe -t -i ../fin.hs -o ../fout.txt

#Input Copland terms(-t) as JSON from the file fout.txt(-i ../fout.txt), write terms as datatypes (-d) to the file fin.hs(-o fin.hs)
genff:
	cd copland-interp ; stack exec -- copland-gen-exe -t -i ../fout.txt -o ../fin.hs -d

#Start an Attestation Server at a random available port
server:
	@cd copland-interp ; stack exec -- copland-server-exe
#Same as make server, but server runs in simulation mode
serversim:
	@cd copland-interp ; stack exec -- copland-server-exe -s

names:
	cd copland-interp ; stack exec -- copland-client-exe -n ../names.txt

namesSim:
	cd copland-interp ; stack exec -- copland-client-exe -n ../names.txt -s

file:
	cd copland-interp ; stack exec -- copland-client-exe -t ../t.hs -e ../ev.hs

fileSim:
	cd copland-interp ; stack exec -- copland-client-exe -t ../t.hs -e ../ev.hs -s

fileNames:
	cd copland-interp ; stack exec -- copland-client-exe -t ../t.hs -e ../ev.hs -n ../names.txt

fileNamesSim:
	cd copland-interp ; stack exec -- copland-client-exe -t ../t.hs -e ../ev.hs -n ../names.txt -s

pzero:
	cd copland-interp ; stack exec -- copland-server-exe -r "3000"

pone:
	cd copland-interp ; stack exec -- copland-server-exe -r "3001"

ptwo:
	cd copland-interp ; stack exec -- copland-server-exe -r "3002"

ghci:
	cd copland-interp ; stack ghci --main-is ${SERV}

ghciserv:
	cd copland-interp ; stack ghci --main-is ${SERV}

ghcisig:
	cd copland-interp ; stack ghci --main-is ${SIG}

ghciclient:
	cd copland-interp ; stack ghci --main-is ${CLIENT}

ghcistore:
	cd copland-interp ; stack ghci --main-is ${STORE}

ghcitestserver:
	cd copland-interp ; stack ghci --main-is ${TESTS}

ghcitestclient:
	cd copland-interp ; stack ghci --main-is ${TESTC}

ghcigen:
	cd copland-interp ; stack ghci --main-is ${GEN}

help:
	@cd copland-interp ; stack exec -- copland-server-exe --help ;
	@echo "" ; echo "" ; echo "" ;
	@cd copland-interp ; stack exec -- copland-client-exe --help ;
	@echo "" ; echo "" ; echo "" ;
	@cd copland-interp ; stack exec -- copland-gen-exe --help ;

helpclient:
	@cd copland-interp ; stack exec -- copland-client-exe --help ;

helpserver:
	@cd copland-interp ; stack exec -- copland-server-exe --help ;

helpgen:
	cd copland-interp ; stack exec -- copland-gen-exe --help

clean:
	cd ./copland-interp/ ; stack clean --verbosity silent
	./repairTarget.sh

.PHONY:	all run clean
