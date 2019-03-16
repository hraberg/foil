# foil

A layered programming language.

## Language Properties

In rough order of importance:

* Layered Design.
** Small Core. (Scheme, Shen, C)
* Gradual Regions -> GC. (Rust, Ada)
** Memory Safety.
* Gradual Static -> Dynamic Typing. (Clojure, Typescript)
** Hindley-Milner Type Inference (OCaml, Rust, Typescript)
* Easy C (or host) Interop. (C, Rust, Graal)
** Predictable Performance.
** SIMD Intrinsics.
** Safe libc Wrapper.
* Lisp Syntax. (Clojure, Scheme)
** Standard library inspired by Clojure / Scheme R7RS + libc.
** Macros.
** REPL: Preferable but Not Key.
* Gradual Purely Functional -> Systems Programming. (Clojure, OCaml, Rust)
** Persistent Data Structures.
** Eager By Default, Lazy Possible.
* Verified Design by Contract. (Ada/SPARK)
* Gradual Systems Programming -> WebAssembly (Rust, Graal)

## Implementation Alternatives

Dependencies to use should be kept at minimum. So the idea of just
depending on GCC for usage is quite tempting. Should be fast.

* Bootstrap (alternatives)
** Clojure / native image?
** Possible self-hosting later on preferable.

* Compilation (alternatives)
** C (or C++) via GCC or Clang.
** Ada via FSF (or GPL) GNAT.
** Rust via rustc.
** LLVM.
** GraalVM for both native image and JVM.

* Dependency Management (alternatives)
** deps.edn.
** Cargo
** Pure git

## Inspirations

In rough order:

* Scheme R7RS
* Cyclone
* Clojure
* Rust
* Ada 2012 / SPARK 2014
* Pre-Scheme
* Shen
* Carp

## References

http://www.ulisp.com/show?3L
https://bitbucket.org/cowan/r7rs/raw/4c27517de187142ad2cf4bcd8cb9199ae1e48c09/rnrs/r7rs.pdf
http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf
http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=C98BFF021F99C3E3C780FDFDA96BE954?doi=10.1.1.3.4031&rep=rep1&type=pdf
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.37.6326&rep=rep1&type=pdf
http://willcrichton.net/notes/gradual-programming/
https://github.com/barak/stalin
https://www.researchgate.net/publication/220606837_A_Retrospective_on_Region-Based_Memory_Management
http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/
http://whiley.org/about/overview/
http://okmij.org/ftp/ML/generalization.html
https://github.com/tomprimozic/type-systems
https://github.com/alehander42/Airtight/blob/master/airtight/hindley_milner_ast.py
https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines
https://www.youtube.com/watch?v=Bp89aBm9tGU&feature=youtu.be
https://github.com/olsner/templisp
https://github.com/andyarvanitis/purescript-native
https://github.com/shedskin/shedskin
https://internals.rust-lang.org/t/pre-rfc-first-class-support-for-compile-to-rust-languages/7610/4
https://github.com/JunSuzukiJapan/macro-lisp
https://github.com/durka/macrolisp
https://github.com/joncatanio/cannoli
https://godbolt.org/ (Compiler Explorer)
https://ferret-lang.org/
https://github.com/Dobiasd/FunctionalPlus
http://www.stroustrup.com/resource-model.pdf
https://www.youtube.com/watch?v=zt0OQb1DBko
https://stackoverflow.com/questions/2067988/recursive-lambda-functions-in-c11

## License

Copyright © 2019 Håkan Råberg.

Distributed under the MIT License.
