.PHONY: all decurio legionarius quickrun

MIX_ENV ?= dev

all: decurio legionarius

decurio:
	mix release --overwrite decurio

legionarius:
	mix release --overwrite legionarius

quickrun: decurio
	_build/$(MIX_ENV)/rel/decurio/bin/decurio start_iex
