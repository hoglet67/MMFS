#!/bin/bash

./build.sh

# A hack, because beebasm doesn't allow string variables
VERSION=`grep "#VERSION#" mmfs100.asm  | cut -d\" -f2 | tr . _`

mkdir -p releases

release=releases/mmfs_${VERSION}_$(date +"%Y%m%d_%H%M").zip
cd build
zip -qr ../${release} *
cd ..
unzip -l ${release}
