#/bin/bash

echo $PWD

chmod u+w ./result/bin/app.jsexe/

mkdir ./result/bin/app.jsexe/r
cp ./resource/*.css ./result/bin/app.jsexe/r

chmod u+w ./result/bin/app.jsexe/index.html 
cp ./resource/index.html ./result/bin/app.jsexe/

