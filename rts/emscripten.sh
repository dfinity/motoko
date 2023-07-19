# Installing emscripten 17
# TODO: We would better have a nix package for emscripten 17.

if [ -z "$EMSDK" ]
then
    if [ ! -d emsdk ]
    then
        git clone https://github.com/emscripten-core/emsdk.git 
        cd emsdk
        git pull
        git checkout "775ba048040f663abbca9ca66e264ee795f64ef3"
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
