#!/usr/bin/env bash

export MIX_ENV=prod

arch="$1"

apt update
apt install -yyq git make

mix local.hex --force
mix local.rebar --force
mix deps.get

make all

for f in $(ls _build/$MIX_ENV/*.tar.gz)
do
  out=${f%%.tar.gz}-${arch}
  out=${out##*/}
  mv "$f" ${out}.tar.gz
done
