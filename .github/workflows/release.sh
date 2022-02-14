#!/usr/bin/env bash

set -xeu

export MIX_ENV=prod
export RELEASE_COOKIE=biscoctus

arch="$1"

apt update
apt install -yyq git make

mix local.hex --force
mix local.rebar --force

make all

for f in $(ls {decurio,legionarius}/_build/prod/*.tar.gz)
do
  out=${f%%.tar.gz}-${arch}
  out=${out##*/}
  mv "$f" ./${out}.tar.gz
done
