#!/usr/bin/env bash

for f in *.comp
do
  prefix=$(echo "$f" | cut -f 1 -d '.')
  glslangValidator -V "$prefix.comp" -o "$prefix.spv"
  spirv-dis "$prefix.spv" -o "$prefix.spvasm"
done
