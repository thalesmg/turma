#!/usr/bin/env bash

export MIX_ENV=prod

arch="$1"

apt update
apt install -yyq git

mix local.hex --force
mix local.rebar --force

mix deps.get

mix release decurio --overwrite
mix release legionarius --overwrite

for f in $(ls _build/prod/*.tar.gz)
do
  out=${f%%.tar.gz}-${arch}
  mv "$f" ${out}.tar.gz
done
