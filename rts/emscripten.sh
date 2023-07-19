# Installing emscripten 17
# TODO: We would better have a nix package for emscripten 17.

if [ -z "$EMSDK" ]
then
    if [ ! -d emsdk ]
    then
        mkdir emsdk
        cp -r $EMSCRIPTEN17/* emsdk
        cd emsdk
        ./emsdk install latest
        ./emsdk activate latest
    else
        cd emsdk
    fi
    source ./emsdk_env.sh
    cd ..
fi
echo "emcc $@"
emcc $@
