This is an emscripten-compiled javascript version of the [Z3 Theorem Prover from Microsoft Research](https://github.com/Z3Prover/z3),
itself available under the MIT license.

This version originates from the [https://github.com/babelsberg/babelsberg-js/](babelsberg.js) - however it's not clear which z3 commit it's based on and which emscripten version was originally used (see [this issue](https://github.com/babelsberg/babelsberg-js/issues/29)) to build it.

When a more recent z3 version (see "(Re)building" below) is built with newest emscripten - the compiled solver works, but crashes on certain queries, such as `(= (a1 9)) (= a1 (* a2 a2))`. More debugging is needed to find the exact cause of the problem.

# (Re)building

In the z3 sources root directory, run:
    
    CXX=em++ CC=emcc python scripts/mk_make.py --x86 --staticlib
    cd build
    sed -i '' 's/AR=ar/AR=emar/g' config.mk
    sed -i '' 's/EXE_EXT=/EXE_EXT=.js/g' config.mk
    sed -i '' 's/^\(LINK_EXTRA_FLAGS=.*\)/\1 -O3/g' config.mk
    emmake make

If everything goes well, after ~20 minutes you should get the z3.js and z3.mem.js

# See also

* https://github.com/babelsberg/z3.js
* https://github.com/normanrz/emscripten-constraints
