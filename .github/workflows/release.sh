#!/usr/bin/env bash

export MIX_ENV=prod

arch="$1"

mix release decurio --overwrite
mix release legionarius --overwrite

for f in $(ls _build/prod/*.tar.gz)
do
  out=${f%%.tar.gz}-${arch}
  mv "$f" ${out}.tar.gz
done
