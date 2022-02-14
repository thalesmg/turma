RELEASE_COOKIE ?= biscoctus
MIX_ENV ?= prod

export RELEASE_COOKIE MIX_ENV

all: decurio legionarius

.PHONY: decurio
decurio:
	cd decurio && \
	  mix release --overwrite

.PHONY: legionarius
legionarius:
	cd legionarius && \
	  mix release --overwrite
