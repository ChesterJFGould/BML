#! /bin/sh

rm -rf release/

mkdir release

find _build | grep \.exe$ | xargs -I {} cp {} release/

cp release_convert.sh release/convert.sh

cp release_tree.sh release/tree.sh

cp example.ml release/

cp example2.ml release/
