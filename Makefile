all: decurio legionarius

.PHONY: decurio
decurio:
	MIX_ENV=prod mix release decurio --overwrite

.PHONY: legionarius
legionarius:
	MIX_ENV=prod mix release legionarius --overwrite
