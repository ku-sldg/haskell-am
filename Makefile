SHELL := /bin/bash
export COPLAND_BUILD=${CURDIR}

ASDF := copland-interp\:exe\:copland-interp-exe
FSDA := copland-interp\:exe\:copland-server-exe
FDSA := copland-interp\:exe\:copland-app-exe
DFAS := copland-interp\:exe\:copland-gen-exe

all:	./copland-interp/copland-interp.cabal
	cd ./copland-interp/ ; stack build

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

ghci:
	cd copland-interp ; stack ghci --main-is ${FSDA}

ghciserv:
	cd copland-interp ; stack ghci --main-is ${ASDF}

ghciapp:
	cd copland-interp ; stack ghci --main-is ${FDSA}

ghcigen:
	cd copland-interp ; stack ghci --main-is ${DFAS}

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

gent:
	cd copland-interp ; stack exec -- copland-gen-exe -n 1 -t -o "fout.hs" -d

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

server:
	@cd copland-interp ; stack exec -- copland-server-exe

serversim:
	@cd copland-interp ; stack exec -- copland-server-exe -s

app:
	cd copland-interp ; stack exec -- copland-app-exe -n ../names.txt -s

appreal:
	cd copland-interp ; stack exec -- copland-app-exe -n ../names.txt

file:
	cd copland-interp ; stack exec -- copland-app-exe -t ../t.hs -e ../ev.hs -s

filereal:
	cd copland-interp ; stack exec -- copland-app-exe -t ../t.hs -e ../ev.hs

filerealnames:
	cd copland-interp ; stack exec -- copland-app-exe -t ../t.hs -e ../ev.hs -n ../names.txt -j

filefakenames:
	cd copland-interp ; stack exec -- copland-app-exe -t ../t.hs -e ../ev.hs -n ../names.txt -s

fakenames:
	cd copland-interp ; stack exec -- copland-app-exe -e ../ev.hs -n ../names.txt -s

client:
	cd copland-interp ; stack exec -- copland-interp-exe -t ../t.hs -e ../ev.hs -c -r "3000" -n ../names.txt

pzero:
	cd copland-interp ; stack exec -- copland-interp-exe -t ../t.hs -e ../ev.hs -v -r "3000" -n ../names.txt

pone:
	cd copland-interp ; stack exec -- copland-server-exe -r "3001"

ptwo:
	cd copland-interp ; stack exec -- copland-server-exe -r "3002"

out:
	cd copland-interp ; stack exec -- copland-interp-exe -t ../t.hs -e ../ev.hs -o ../res.txt -s

outreal:
	cd copland-interp ; stack exec -- copland-interp-exe -t ../t.hs -e ../ev.hs -o ../res.txt

run:
	cd copland-interp ; stack exec -- copland-app-exe -w -a

runSim:
	cd copland-interp ; stack exec -- copland-app-exe -s -w -v

term:
	cd copland-interp ; stack exec -- copland-app-exe -w -t ../t.hs

names:
	cd copland-interp ; stack exec -- copland-app-exe -s -n "../names.txt"

namest:
	cd copland-interp ; stack exec -- copland-app-exe -s -n "../names.txt" -t ../t.hs

clean:	
	cd ./copland-interp/ ; stack clean --verbosity silent
	./repairTarget.sh

.PHONY:	all run clean
