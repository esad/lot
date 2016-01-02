This is an emscripten-compiled javascript version of the [Z3 Theorem Prover from Microsoft Research](https://github.com/Z3Prover/z3),
itself available under the MIT license.

# (Re)building

In the z3 sources root directory, run:
    
    CXX=em++ CC=emcc python scripts/mk_make.py --x86 --staticlib
    cd build
    sed -i '' 's/AR=ar/AR=emar/g' config.mk
    sed -i '' 's/EXE_EXT=/EXE_EXT=.js/g' config.mk
    sed -i '' 's/^\(LINK_EXTRA_FLAGS=.*\)/\1 -O3/g' config.mk
    emmake make

If everything goes well, after ~20 minutes you should get the z3.js and z3.mem.js

# Similar Efforts

* https://github.com/babelsberg/z3.js
* https://github.com/normanrz/emscripten-constraints
