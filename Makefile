ELM_MAKE_FLAGS ?= --optimize
export PATH := ${PWD}/bin:${PATH}

.PHONY: all elm
all: bin/elm elm

bin/elm:
	# https://github.com/elm/compiler/tree/master/installers/mac
	curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-mac-64-bit.gz
	# https://github.com/elm/compiler/tree/master/installers/linux
	#curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
	gunzip elm.gz
	chmod +x elm
	mkdir -p bin
	mv elm bin

elm:
	bin/elm make src/Flash.elm        ${ELM_MAKE_FLAGS} --output build/elm.js
	bin/elm make src/Mannequin.elm    ${ELM_MAKE_FLAGS} --output build/mannequin.js
