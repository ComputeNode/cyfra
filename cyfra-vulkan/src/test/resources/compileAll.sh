#!/bin/bash

for f in *.comp
do
  prefix=$(echo "$f" | cut -f 1 -d '.')
  glslangValidator -V "$prefix.comp" -o "$prefix.spv"
done
