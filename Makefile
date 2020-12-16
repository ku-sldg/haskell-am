SHELL := /bin/bash
export COPLAND_BUILD=${CURDIR}

SERV := copland-interp\:exe\:copland-server-exe
APP := copland-interp\:exe\:copland-app-exe
GEN := copland-interp\:exe\:copland-gen-exe
SIG := copland-interp\:exe\:sig-server-exe

all:	./copland-interp/copland-interp.cabal
	cd ./copland-interp/ ; stack build

run:
	cd copland-interp ; stack exec -- copland-app-exe -w -a

run1:
	cd copland-interp ; stack exec -- copland-app-exe -w -n ../names.txt


startCS:
	cd copland-interp ; stack exec connection-server-exe

startSIG:
	cd copland-interp ; stack exec sig-server-exe

runSim:
	cd copland-interp ; stack exec -- copland-app-exe -s -w -v

term:
	cd copland-interp ; stack exec -- copland-app-exe -w -t ../t.hs

termCompiled:
	cd copland-interp ; stack exec -- copland-app-exe -w -compile -t ../t.hs -a

termCompiledd:
	cd copland-interp ; stack exec -- copland-app-exe -compile -t ../t.hs

attack:
	./modTarget.sh
repair:
	./repairTarget.sh

provision:
	cd copland-interp ; stack exec -- copland-app-exe -p

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
	cd copland-interp ; stack exec -- copland-app-exe -n ../names.txt

namesSim:
	cd copland-interp ; stack exec -- copland-app-exe -n ../names.txt -s

file:
	cd copland-interp ; stack exec -- copland-app-exe -t ../t.hs -e ../ev.hs

fileSim:
	cd copland-interp ; stack exec -- copland-app-exe -t ../t.hs -e ../ev.hs -s

fileNames:
	cd copland-interp ; stack exec -- copland-app-exe -t ../t.hs -e ../ev.hs -n ../names.txt

fileNamesSim:
	cd copland-interp ; stack exec -- copland-app-exe -t ../t.hs -e ../ev.hs -n ../names.txt -s

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

ghciapp:
	cd copland-interp ; stack ghci --main-is ${APP}

ghcigen:
	cd copland-interp ; stack ghci --main-is ${GEN}

help:
	@cd copland-interp ; stack exec -- copland-server-exe --help ;
	@echo "" ; echo "" ; echo "" ;
	@cd copland-interp ; stack exec -- copland-app-exe --help ;
	@echo "" ; echo "" ; echo "" ;
	@cd copland-interp ; stack exec -- copland-gen-exe --help ;

helpclient:
	@cd copland-interp ; stack exec -- copland-app-exe --help ;

helpserver:
	@cd copland-interp ; stack exec -- copland-server-exe --help ;

helpgen:
	cd copland-interp ; stack exec -- copland-gen-exe --help

clean:
	cd ./copland-interp/ ; stack clean --verbosity silent
	./repairTarget.sh

.PHONY:	all run clean
