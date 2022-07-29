#!/usr/bin/env bash

export MIX_ENV=prod

arch="$1"

make all

for f in $(ls _build/prod/*.tar.gz)
do
  out=${f%%.tar.gz}-${arch}
  mv "$f" ${out}.tar.gz
done
