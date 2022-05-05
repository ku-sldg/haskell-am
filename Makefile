SHELL := /bin/bash
export COPLAND_BUILD=${CURDIR}

#MAIN := copland-interp\:exe\:copland-interp-exe
#SERV := copland-interp\:exe\:server-main-exe
#CLIENT := copland-interp\:exe\:client-main-exe
#GEN := copland-interp\:exe\:gen-main-exe

MAIN := copland-interp-exe
SERV := server-main-exe
CLIENT := client-main-exe
GEN := gen-main-exe

EXE_PRE := copland-interp\:exe\:

all:	./copland-interp/copland-interp.cabal
	cd ./copland-interp/ ; stack build

build_client :	./copland-interp/copland-interp.cabal
		cd ./copland-interp/ ; stack build ${EXE_PRE}${CLIENT}

build_server :	./copland-interp/copland-interp.cabal
		cd ./copland-interp/ ; stack build ${EXE_PRE}${SERV}

run:
	cd copland-interp ; stack exec -- ${CLIENT} --appraise --spawnCvms --spawnAsps
#--spawnAsps #--spawnCvms   -t "inputs/phrase.hs" -e "inputs/initEv.hs" --spawnAsps --appraise

ghci:
	cd copland-interp ; stack ghci --main-is ${EXE_PRE}${MAIN}

ghciserver:
	cd copland-interp ; stack ghci --main-is ${EXE_PRE}${SERV}

ghciclient:
	cd copland-interp ; stack ghci --main-is ${EXE_PRE}${CLIENT}

ghcigen:
	cd copland-interp ; stack ghci --main-is ${EXE_PRE}${GEN}

runaspserver_default:
	cd copland-interp ; stack exec -- ${SERV} -r "ASP_DEFAULT" -a 42

runaspserver_cache:
	cd copland-interp ; stack exec -- ${SERV} -r "CACHE" -a 4

runaspserver_attest:
	cd copland-interp ; stack exec -- ${SERV} -r "ATTEST" -a 1

runaspserver_appraise:
	cd copland-interp ; stack exec -- ${SERV} -r "APPRAISE" -a 2

runaspserver_cert:
	cd copland-interp ; stack exec -- ${SERV} -r "CERT" -a 3

runparserver_one:
	cd copland-interp ; stack exec -- ${SERV} -r "ASP_DEFAULT" -a 42

runserver_zero:
	cd copland-interp ; stack exec -- ${SERV} -r "CVM_0" -p 1 -c "SIG_0"

runserver_one:
	cd copland-interp ; stack exec -- ${SERV} -r "CVM_1" -p 1 -c "SIG_1"

runserver_two:
	cd copland-interp ; stack exec -- ${SERV} -r "CVM_2" -p 2 -c "SIG_2"

runserver_three:
	cd copland-interp ; stack exec -- ${SERV} -r "CVM_3" -p 2 -c "SIG_3"

runserver_four:
	cd copland-interp ; stack exec -- ${SERV} -r "CVM_4" -p 2 -c "SIG_4"

runsigserver_zero:
	cd copland-interp ; stack exec -- ${SERV} -r "SIG_0" --sign_server

runsigserver_one:
	cd copland-interp ; stack exec -- ${SERV} -r "SIG_1" --sign_server

runsigserver_two:
	cd copland-interp ; stack exec -- ${SERV} -r "SIG_2" --sign_server

runaspserver_one:
	cd copland-interp ; stack exec -- ${SERV} -r "ASP1" -a 1

run1:
	cd copland-interp ; stack exec -- ${CLIENT} -w -n ../names.txt

runSim:
	cd copland-interp ; stack exec -- ${CLIENT} -s -w -v

attack:
	./modTarget.sh
repair:
	./repairTarget.sh

provision:
	cd copland-interp ; stack exec -- ${CLIENT} -p


#Generate one random(-n 1) Copland term(-t) as a datatype(-d)
gen:
	cd copland-interp ; stack exec -- ${GEN} -n 1 -t -d

#Generate one random(-n 1) Copland term(-t) as JSON
genj:
	cd copland-interp ; stack exec -- ${GEN} -n 1 -t

#Input Copland terms(-t) as datatypes from stdin, write JSON to stdout
geni:
	cd copland-interp ; stack exec -- ${GEN} -t

#Input Copland terms(-t) as JSON from stdin, write terms as datatypes(-d) to stdout
gend:
	cd copland-interp ; stack exec -- ${GEN} -t -d

#Input Copland terms(-t) from the file fin.hs(-i ../fin.hs) as datatypes, write JSON to the file fout.txt(-o ../fout.txt)
genf:
	cd copland-interp ; stack exec -- ${GEN} -t -i ../fin.hs -o ../fout.txt

#Input Copland terms(-t) as JSON from the file fout.txt(-i ../fout.txt), write terms as datatypes (-d) to the file fin.hs(-o fin.hs)
genff:
	cd copland-interp ; stack exec -- ${GEN} -t -i ../fout.txt -o ../fin.hs -d

#Start an Attestation Server at a random available port
server:
	@cd copland-interp ; stack exec -- ${SERV}
#Same as make server, but server runs in simulation mode
serversim:
	@cd copland-interp ; stack exec -- ${SERV} -s

help:
	@cd copland-interp ; stack exec -- ${CLIENT} --help ;
	@echo "" ; echo "" ; echo "" ;
	@cd copland-interp ; stack exec -- ${SERV} --help ;
	@echo "" ; echo "" ; echo "" ;
	@cd copland-interp ; stack exec -- ${GEN} --help ;

helpclient:
	@cd copland-interp ; stack exec -- ${CLIENT} --help ;

helpserver:
	@cd copland-interp ; stack exec -- ${SERV} --help ;

helpgen:
	cd copland-interp ; stack exec -- ${GEN} --help

clean:
	cd ./copland-interp/ ; stack clean --verbosity silent
	./repairTarget.sh

.PHONY:	all run clean
