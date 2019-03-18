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

## Reflections After First Week

Foil now compiles to C++17 after a few days of initially compiling to
C11. C is simpler and compiles faster, but requires much more initial
development to add features. Compiling to modern C++ instead gives us:

* Local type deduction via the `auto` keyword.
* Lambdas, including closures.
* Mix of static and on-demand typing of function signatures by
  refinement via templates. This makes it possible to omit specifying
  many non-local types as well.
* Smart pointers for managing memory via RIAA.
* Mutable containers (collections) via STL.

The compiler is written in Clojure, and the language is read using
`tools.reader`, which implies that Foil is source compatible with
Clojure. Statements are wrapped into lambdas when necessary to become
expressions. The compiler is single pass and there's no IR yet, and it
directly generates C++ as it walks the s-expression tree.

Macros are implemented via a multi-method. There are a few special
forms, and special handling of top-level and to allow statements
within blocks without being wrapped. It supports `def`, `defn`,
`defstruct`, `fn`, `loop/recur`, `binding` and `let`. The compiler
also supports a special form `$code`, which emits a verbatim string
into the generated source.

The compiler generates STL containers for literal vectors, list, maps
and sets. It also supports regex and date literals. Basic operators
are compiled directly, and have no function wrappers for now. Keywords
are compiled to strings. Raw arrays are not yet supported, but can be
indirectly constructed as vector literals.

Types are specified using normal Clojure tags, `^int x`. If there's no
type it defaults to `auto`. A type can also be a string, in which case
it's used verbatim in the generated C++, to simplify usage of more
complex signatures. For collection literals the type specifies the
type contained. Modifiers `:const` and `:dynamic` compiles to `const`
and `thread_local` in C++.

All generated functions are normal C++ template functions (or lambdas)
so interop is the default. Member functions and fields can be called /
accessed using the normal `.` prefix syntax. Structs can be created
via postfix `.`. Currently all structs are created on the stack (no
`new` keyword), and needs to be explicitly moved to the heap.

Namespaces compiles to C++ namespaces. This area is quite basic at the
moment. `:require/:include` is supported and are compiled to
`#include` directives.

There is no runtime or GC by design. Currently there's also no memory
safety, as simply using smart pointers cannot alone guarantee it. How
to solve this is open for debate. Exactly how much of the underlying
machinery to expose is also an open question. I expect that there will
be a `foil.core` library that will use interop extensively, but that
user code will mainly depend on this and not usually see any C++
except for the when using libraries within the sub-domain they operate
in. The goal is not to support any random C++ construct in the
language, just leverage them where it makes sense. For certain usages,
one would have to rely on `$code` or write native bridge code.

Currently all Foil code is read from standard in and C++ is written to
standard out. For ease of use, this currently adds a main function if
necessary. One has to manually generate individual `.hpp` files and a
`.cpp` file for the file with the main method and compile them.

We could support https://github.com/arximboldi/immer which is a C++
library for persistent data structures. In practice this would involve
a wrapper and some ability to decide what type literals
generate. Forcing a hard dependency on this library goes against the
layered approach.

My idea is to evolve this for a bit until it can do things without
worrying about portability. At a later point there could be time to
split the compiler up and add a second backend, so the direct
dependency on C++ is minimised.

The compiler is currently written in Clojure, and no attempt has been
made to make this easily portable, and Foil-Clojure compatibility
isn't a goal. The aim is to eventually migrate the compiler to a
commonly supported subset of Foil and Clojure so it can become
self-hosting. This will be done in part by adding features to Foil so
it can compile itself, and in part to simplify the Clojure used by the
compiler to make it easier to port. Self-hosting isn't a short-term
goal.

As mentioned above, Foil macros are implemented via a multi-method,
but this will likely be rewritten to use a more basic subset of
Clojure, but the idea is to also extend it with support for
Scheme-style `syntax-rules`. This way macros can be defined in a
declarative way in Foil itself, without the need for new code to be
executed at compile time. But some low-level macros will still be
implemented in the compiler, just not in an extensible way.

This common subset of Clojure and Foil should eventually be well
defined. This doesn't necessarily mean support for reader conditionals
or `.cljc` in Foil, just defining a set that should work on both so
one can consciously choose to target writing code in this set for
certain use cases.

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
https://github.com/rusthon/Rusthon
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
https://github.com/arximboldi/immer
https://github.com/nlohmann/json

## License

Copyright © 2019 Håkan Råberg.

Distributed under the MIT License.
