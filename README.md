# foil

A small compiled and statically typed Lisp.

## Building

Depends on a C++14 compiler, Java and Leiningen being on the path.

```sh
make check
```

## Language Properties

In rough order of importance:

* Layered Design.
  * Small Core. (Scheme, Shen, C)
* Gradual Regions -> GC. (Rust, Ada)
  * Memory Safety.
* Gradual Static -> Dynamic Typing. (Clojure, Typescript)
  * Hindley-Milner Type Inference (OCaml, Rust, Typescript)
* Easy C (or host) Interop. (C, Rust, Graal)
  * Predictable Performance.
  * SIMD Intrinsics.
  * Safe libc Wrapper.
* Lisp Syntax. (Clojure, Scheme)
  * Standard library inspired by Clojure / Scheme R7RS + libc.
  * Macros.
  * REPL: Preferable but Not Key.
* Gradual Purely Functional -> Systems Programming. (Clojure, OCaml, Rust)
  * Persistent Data Structures.
  * Eager By Default, Lazy Possible.
* Verified Design by Contract. (Ada/SPARK)
* Gradual Systems Programming -> WebAssembly (Rust, Graal)

## Implementation Alternatives

Dependencies to use should be kept at minimum. So the idea of just
depending on GCC for usage is quite tempting. Should be fast.

* Bootstrap (alternatives)
  * Clojure / native image?
  * Possible self-hosting later on preferable.

* Compilation (alternatives)
  * C (or C++) via GCC or Clang.
  * Ada via FSF (or GPL) GNAT.
  * Rust via rustc.
  * LLVM.
  * GraalVM for both native image and JVM.

* Dependency Management (alternatives)
  * deps.edn.
  * Cargo
  * Pure git

## Inspirations

In rough order:

* Scheme R7RS
* Cyclone
* Clojure
* Rust
* ParaSail
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

## Reflections After Second Week

Foil has mainly grown and gotten more features and the core has been
split out to a `.hpp` file written in Foil. Many new (and improved)
macros and functions. Some support for atoms and smart pointers.

But the details of this can be seen in the code and in the git
history. Let's instead reflect on the initial points set out at day
one, as Foil has somewhat drifted into something more concrete.

### Layered Design.
#### Small Core. (Scheme, Shen, C)

The core is small, and so is the compiler. But most of it's not
currently reusable in any real sense, as there's a lot of things that
tie both to C++ at the moment. The compiler still just walks the Lisp
code and generates C++ directly without any intermediate passes or
representation.

### Gradual Regions -> GC. (Rust, Ada)
#### Memory Safety.

Foil embraces normal C++ RIAA and by default creates all data on the
stack. Smart pointers can be used to create data on the heap. Raw
pointer and array access is possible. So this area is still very
basic.

I'm considering to introduce `unsafe` blocks, and disallow (explicit)
pointer and array access outside it (and potentially also
interop). Foil doesn't support `nil` directly, but functions may still
return `nullptr` during interop and some functions are inherently
unsafe, like `front` on an empty vector for example.

There's an example of `first-opt` which uses `std::optional`, but
going down this route is quite intrusive, but would move the language
closer to OCaml or Rust. Pattern matching then becomes if not needed,
at least desirable.

### Gradual Static -> Dynamic Typing. (Clojure, Typescript)
#### Hindley-Milner Type Inference (OCaml, Rust, Typescript)

Currently this is all done via `auto` type deduction and
templates. Many places require explicit types, especially when it
comes to collection element types. Templates make the compilation
slower, but in overall this area works quite well.

C++ has quite confusing (or elaborate) reference, move and copy
semantics, so it's bound to have some issues currently. By default
local variables are created as by value and function template
parameters as by reference, this is a bit ad-hoc.

### Easy C (or host) Interop. (C, Rust, Graal)
#### Predictable Performance.
#### SIMD Intrinsics.
#### Safe libc Wrapper.

As Foil compiles directly to C++, all this is potentially alreaday
there. There's no safe wrappers yet, and performance isn't necessarily
predicable or tuned. Foil has a `$code` macro the allows to splice in
verbal host (C++) code into the generated output.

### Lisp Syntax. (Clojure, Scheme)
#### Standard library inspired by Clojure / Scheme R7RS + libc.
#### Macros.
#### REPL: Preferable but Not Key.

Apart from REPL all this is true, but macros still have to be written
using Clojure. `foil.core` is inspired by `clojure.core` and is a
superset of a subset. For a while I aimed to keep it R7RS inspired as
well, but decided to stick mainly to Clojure as it created too much
duplication and confusion in a small language.

### Gradual Purely Functional -> Systems Programming. (Clojure, OCaml, Rust)
#### Persistent Data Structures.
#### Eager By Default, Lazy Possible.

Foil currently only support eager, mutable collections, but it does
also support the basic higher order functions expected from functional
programming. Unlike Clojure, variables can be declared to be mutable
by adding `^:mut`.

### Verified Design by Contract. (Ada/SPARK)

Nothing done here yet.

### Gradual Systems Programming -> WebAssembly (Rust, Graal)

Foil still only compiles to C++.

## Reflections After Third Week

### Changes

This week was quite slow, the main thing is that I've ditched
`std::forward_list` for `foil::core::ConsList` which is a normal cons
list implemented in Foil itself. This lead to allowing for usage of a
few more C++ constructs so one could express the iterators etc.
directly in Foil. This list uses `std::shared_ptr` to allow for
sharing of tails.

I've added `:unsafe` meta data which is necessary to use to compile
operations using raw pointers.

I've also added the start of a small set of testing utilities, with
the aim to refactor `example.cljc` into `tests.cljc` and group it into
reasonable tests.

`doseq` and `for` now support the expected directives like `:let`,
`:while` and `:when`.

Allowing `|` to represent `,` in tags which removes the need for most
(but not all) use of string tags.

### Outlook

At this point Foil is quite complete for what it is. Many library
functions could be added (and potentially tranducers etc.), but the
semantics of the language is now quite clear. It fully embraces RIAA
and use of smart pointers, and compiles down to quite readable C++.

The compiler is still single pass, and I've let the formatting slide a
bit, as that's actually one of the harder things to keep track of
while generating the output.

My next steps will mainly involve cleaning up the tests and then
decide what to do next. I've started to read about region based memory
management and doing more research again, and there are several ways
from here that could be taken:

1. Evolve Foil as is and just add more features (see above).
2. Try to add custom region based allocators and do more analysis when
   compiling to aim for memory safety.
3. Decouple the compiler front end better from the code generator.
4. Redo the entire thing and target Rust instead of C++. Rust doesn't
   have templates the same way C++ has, so the compilation strategy
   (or language semantics) likely must change a bit. My impression is
   that generating the correct lifetimes will be quite hard, but this
   would potentially still be easier than the next option.
5. Retarget to something simpler, like C+libc or WASM+WASI. This
   requires more work in the compiler, but gives full control over the
   semantics. This requires doing proper type and lifetime analysis /
   inference.
6. Consider Truffle/JVM.
7. Back to the drawing board.

As it stands, Foil isn't very portable, as it's heavily tied to C++,
RIAA and STL. Some analysis what one wants to achieve is needed before
pushing further. There are two slightly conflicting goals at play,
creating a small non-GC Lisp that allows for systems programming, and
creating a small Lisp that can be ported easily to different targets.

One implicit goal here is to tease out the minimal useable subset of
Clojure one wants to have available in this core. If this subset is
small enough, and coupled with a portable test suite, reuse might not
be that necessary as it's potentially easy to rewrite it for each
target. More advanced features can then be built on and share the
core.

## Reflections After Fourth Week

The biggest change was porting the tests to the new testing framework
which is a small subset of `clojure.test`. I also tweaked how
recursive function calls are done from using a static field in the
function struct to a local variable pointing to itself. This was due
to various template issues and simply worked better.

I've added several more of the normal sequence functions, and I also
added a small spike of how transducers could look, but this is far
from finished. I did another spike (in embedded C++) allowing keywords
to be used like functions. This worked, but backed it out as it would
have required a lot of extra features to be added to be possible to be
expressed directly in Foil.

I did add `:ref` for returns and made `:val` the implicit default, to
avoid one doesn't accidentally return a local reference. This means
`auto` is the default in C++, which will return the raw type, while
`:ref` depends on `decltype(auto)` to maintain potential reference
qualifiers. I'm not 100% about this, but the basics seems to work. The
general idea is that you should only ever return a reference if it was
given to you as an argument.

In general, as mentioned last week, I see phase one of Foil now being
more or less complete, and it's time to take a step back and
reflect. So the outlook above still stands, but let me add a
retrospective and some open questions.

### Retrospective and Questions

1. How much is actually copied? I don't fully understand the C++ move
   semantics, containers and references. Java and C are both clear,
   but C++ feels more complex.
2. But C++ and templates gives us a lot. That said, the amount of C++
   and template parameters that are necessary in `foil.core` makes it
   hard to reuse things for different targets, or impose your own
   style. This isn't necessarily a problem, but the question then
   becomes how much one should embrace C++. One is often forced to
   create more overloads of a function than one wants to deal with
   universal references.
3. Template errors aren't necessarily complicated to fix, but often
   extremely verbose, so it can be hard to figure out what the actual
   problem is.
4. The confusion over values, references and RIAA makes it hard to
   make any firm statements on what semantics the language actually
   has. This might not be an issue and can likely be incrementally
   improved. Working a bit with someone who knows C++ well to do a
   review would be useful.
5. Related to the above, it makes it hard to know if it's worth the
   effort to try to get region based memory management into the C++
   version. C++ has its own allocator concept. But it feels hard to do
   anything clever here before one understands the foundations fully.

I'm happy where Foil ended up, and think the current version has
promise. But that said, I'm parking it for a bit to explore the
problem from a different angle. I'm going back to the original C
roots, and started working on Hindley Milner type inference and hope
to compile this to a small Lisp with lambdas but without closures (for
now) to C, and then explore adding region inference into the same
language, using similar tree walking analysis. Doing the C++ version
of Foil first, which has been easier to add features to and explore,
will help guide the next phase.

#### Memory Management

The idea in Foil is to use the stack as much as possible, and only
return references that were given as parameters, this is usually done
in functions that modify its input, like `conj!` and to allow
chaining. The assumption is the return value optimisation in C++ will
make creating and returning values cheap enough.

Sometimes this pattern leads to undefined behaviour though, as
functions like `conj!` mentioned above might return a reference to a
local variable in a way one didn't intend, and which might then get
returned again from the enclosing function by mistake. I've changed
the default to return by value to avoid this as much as possible.

C++ distinctions between lvalues and rvalues also complicates things
and can also lead to undefined (or at least unexpected) behaviour, as
well as leading to duplication as one has to ensure there are
overloads taking universal references available in some cases.

The way move and copy assignment works in C++ is also somewhat
confusing, as coming from Java or Clojure, local variables don't
always behave as one expect.

## Name

Unknown to me, turns out there are (at least) a few things called
foil, a couple of old programming languages from the 60-70s,
https://en.wikipedia.org/wiki/FOIL_(programming_language) but also a
pre-Clojure project by Hickey: http://foil.sourceforge.net/ (!)

So the name should be seen as a project name only, not the eventual
name of any eventual language.

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

https://github.com/eliben/code-for-blog/blob/master/2018/type-inference/typing.py

http://dev.stephendiehl.com/fun/006_hindley_milner.html

https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines

https://www.youtube.com/watch?v=Bp89aBm9tGU&feature=youtu.be

http://matt.might.net/articles/c++-template-meta-programming-with-lambda-calculus/

http://vitiy.info/templates-as-first-class-citizens-in-cpp11/

https://github.com/olsner/templisp

https://github.com/andyarvanitis/purescript-native

https://github.com/shedskin/shedskin

https://github.com/serge-sans-paille/pythran

https://github.com/rusthon/Rusthon

https://internals.rust-lang.org/t/pre-rfc-first-class-support-for-compile-to-rust-languages/7610/4

https://github.com/JunSuzukiJapan/macro-lisp

https://github.com/durka/macrolisp

https://github.com/joncatanio/cannoli

https://godbolt.org/

https://ferret-lang.org/

https://github.com/Dobiasd/FunctionalPlus

http://www.stroustrup.com/resource-model.pdf

https://www.youtube.com/watch?v=zt0OQb1DBko

https://stackoverflow.com/questions/2067988/recursive-lambda-functions-in-c11

https://github.com/arximboldi/immer

https://github.com/Ableton/atria

https://github.com/solodon4/Mach7

https://github.com/nlohmann/json

https://gist.github.com/foonathan/023ff0fe923c6b0312dfc15e17ebb595

http://tutok.sk/fastgl/callback.html

https://vittorioromeo.info/index/blog/passing_functions_to_functions.html

https://brycelelbach.github.io/cpp17_features/

https://medium.com/@knappador/why-the-machine-b9803a77fa29

https://rust-unofficial.github.io/too-many-lists/third-final.html

https://github.com/ericniebler/range-v3

https://github.com/nrc/r4cppp

https://github.com/densh/scala-offheap

https://www.youtube.com/watch?v=bNWTRdAlwUs

https://arxiv.org/pdf/1902.00525v1.pdf

https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-908.pdf

https://people.eng.unimelb.edu.au/schachte/papers/mspc12.pdf

https://github.com/plasma-umass/mesh

https://di.ku.dk/forskning/Publikationer/tekniske_rapporter/2003/03-10.pdf

http://okmij.org/ftp/Computation/typeclass.html

http://dev.stephendiehl.com/fun/000_introduction.html

http://www.stephendiehl.com/llvm/

https://xavierleroy.org/mpri/2-4/

## License

Copyright © 2019 Håkan Råberg.

Distributed under the MIT License.
