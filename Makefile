.SILENT:

.PHONY: install clean

default: build

FILE ?= ""

RUN_ARGS =
ifneq (${FILE}, "")
	RUN_ARGS = exes -- ${FILE}
endif

run:
	cabal run ${RUN_ARGS}

test:
	cabal test

build:
	echo "Building cabal project..."
	cabal build

install:
	cabal update
	cabal install alex happy BNFC --overwrite-policy=always

clean: 
	cabal clean