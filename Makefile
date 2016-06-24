# make      = make build/main.js -- build main.js
# make all  = make rebuild       -- remove build and artificats, build main.js
# make clean                     -- remove build artificacts and elm-stuff
#
# make server                    -- run postgrest and http-server dtached
# make depcheck                  -- test we have all we need

build/main.js: src/Main.elm 
	[ ! -r build ] && mkdir build
	elm make src/Main.elm --output build/main.js

cleanbuild:
	[ -d build ] && rm -r build || echo "no build dir"
	[ -d elm-stuff/build-artifacts/0.17.0/user ] && rm -rf elm-stuff/build-artifacts/0.17.0/user || echo "no artifact user dir"

rebuild: cleanbuild build/main.js 

all: rebuild
	
clean:  cleanbuild
	[ -d elm-stuff ] && rm -r elm-stuff || echo "no elm-stuff dir"


server:
	dtach -n postgrest postgrest postgres://postgres:postgres@10.145.65.240:5432/lncddb -p 3003 -a postgres
	dtach -n httpserv http-server


ELMVERSION=$(shell elm-make --version 2>&1 | awk '( NR == 1 ){print $$2}')
depcheck:
	which dtach
	which postgrest
	which http-server
	which elm
	[ ${ELMVERSION} = "0.17" ]

