# Description

The support of modular features is triggered by the command-line
option `-mjazz`. With that option enabled, the Jasmin compiler shall
handle the following additional syntactic constructs:

 1. a `modsignature` declaration as the first declaration of the
    module. It would mean that the module is "parametric" on the given
    set of parameters. The `modsignature` declaration allows three kinds of
    parameter declaration: 1) `param` (`int`) parameters, which are
    allowed appear in array sizes; 2) `global <ptype>` for constants;
    and 3) `fn <func> ( <ptype>* ) -> <ptype>*` for functional
    parameters. An example would be:
    ```
	modsignature {
	  param int nlimbs;
	  global u64[nlimbs] modulus;
	  fn mulm ( u64[limbs], u64[nlimbs] ) -> u64[nlimbs];
    }
	```
 2. an extended `require` clause, allowing the qualification of the
    import (`as <name>`), and the instantiation of module's parameters
    (`with { p1=id1; ...; pn=idn; }`). An example would be:
	```
	param int nlimbs = 10;
	u64[nlimbs] glob_P = { ..... };
	inline fn mul(stack u64[nlimbs] a b) -> stack u64[nlimbs] {...}
	from MJazz require "bn_x" as "BN"
		with { nlimbs = nlimbs;
   	           modulus = glob_P;
			   mulm = mul; }
	```
    Notice that, for now, we restrict the right-hand side of the
    assignments on the `with` clause to be previously defined
    identifiers.
 3. The use of qualifiers when referencing to symbols in
    expressions. An example would be:
	```
	x = BN.mulU( x, BN.glob_P);
	```

**Remark:** Parametric modules **should** always be imported instantiating the
complete set of parameters (a complete `with` clause), as specified in
the `modsignature` declaration of the imported module (but possibly
with own parameters...).

**Remark:** require clauses **can** also qualify the import, in which
case definitions would be accessed through qualified
identifiers. Notice however that qualifiers are used to disambiguate
the use of identifiers -- they do not enforce, *per se*, different
clones of the objects. In this regard, they differ from the
*namespaces* mechanism (see discussion bellow...).


# Uses of `jasminc -mjazz ...`

There are two "modes" of operation when invoking `jasminc -mjazz`:

 1. To produce an EC model of the (possibly parametric) module (option
    `-mEC`). The outcome would be an EC theory (abstract, in the case of a
    parametric module) that include all definitions of the module file
    (and only those). All needed theories shall be included as
    sub-theories (ensuring proper sharing as needed...).
	
 2. To produce a plain-Jasmin AST with the flattening of a
    (necessarily) non-parametric module that would be further
    processed by the remaining compiler passes.

## Context of inner-modules

Each module (parametric or not) **is assumed to be interpreted in an
empty context**. This is a departure from "plain" Jasmin programs,
where `require` clauses can be used to import modules depending on
some previous declarations [in fact, a typical design-pattern used to
emulate parametric modules in plain Jasmin...].

Of course, this means that `jasminc -mjazz` **is not** backwards
compatible with plain `jasminc`. On the other hand, the whole point of
designing modules is to take a more structured approach for those
awkward code-patterns that are often difficult to reason about.


# Full-identifiers

The idea behind the handling of the proposed modular features is to
inline and expand all definitions of (possibly parametric) modules,
resolving all uses of *identifiers* in what we call
**full-identifiers**. 

## Diamond imports and name-clashes

In plain Jasmin, complex import chains are allowed. However, these are
unproblematic because multiple instances of the same symbol can be
safely merged. As an example, consider the following diamond-shaped
sequence of imports:

```
// MODULE D
fn fd(...){...};
...
---
// MODULE B
require D
...
---
// MODULE C
require D
...
---
// MODULE A
require B
require C
...(uses fd) // refers to B.D.fd or C.D.fd ???
```

The question of whether `fd` symbol is accessed through module `B` or
`C` (via `D`) really doesn't matter -- they end up being the same
object (a single assembly definition). We can say that the **full
identifier** for `fd` in the context of module `A` is indeed `D.fd` --
the distinct import paths leading to its definition only adds
redundancy to the "visibility" of the symbol `fd` from the perspective
of module `A`. Summing up, *full identifiers* in plain Jasmin are
always flat: `ModuleName.Ident`.

It is instructive at this point to look at a close issue related to
*name-clashing*. If two modules (say `A` and `B`) define some `fX`,
the compiler would emit an error (name clash of `fX`). This is solely
a limitation of the scoping rules adopted by the syntax of the
language (that, in turn, simplifies the naming of symbols of the
generated code). If needed/intended, it could be handled by some
name-spacing mechanism -- "semantically", there would be no problem
on allowing to access both `A.fX` and `B.fX`, here distinguished by
what we have called their *full identifiers* (but, of course, the
compiler would need to add module prefixes to both symbols...).

## Turning to Parametric-modules

When we consider parametric modules, in examples such as the above
diamond-shaped imports, we might reach two distinct instances
of module `D`!!!

```
// MODULE D(P)
fn fd;
...
---
// MODULE B
...
require D(P1)
...
---
// MODULE C
...
require D(P2)
...
---
// MODULE A
require B
require C
...(uses "fd") // ??? "fd" refers to B.D(B.P1).fd or C.D(C.P2).fd ???
```

Now, `B.D(B.P1).fd` and `C.D(C.P2).fd` might or might not be the "same object" (we need to decide whether `B.P1` is equal to `C.P2`), and things get much
more intricate when we allow `A, B, C` to be parametric modules
themselves. What we can say is, in the context of some toplevel
(non-parametric) module such as `A`, we can assign a *full identifier*
to all definitions possibly used by `A`. We shall extend the
previously introduced notion of *full identifiers* for parametric
modulus such as `D` to the shape `D(p1...pn).fd`, where
`p1..pn` are **resolved parameters**. *Resolved parameters* are
defined according to the structure of the parameters:
 - resolved `param int` parameters are integer literals;
 - resolved `global`parameters and `fn` parameters are the *full
   identifiers* of that instance.
   
In the previous example, full indentifiers would be things like
`A.fa`, `B.globB`, `D(B.globB).fd`, `D(C.globC).fd`. In this example,
the parameter for module `D` would be a global constant, and we see
that the `fd` procedure of both instances of `D` should not be
identified. If, on the other hand, the parameter of `D` happens to be
an integer, and both module `B` and `C` instantiate it with `5`, then
we would end up with the full identifier `D(5).fd` (i.e. is the same
definition).

The problem is how do we give access to `fd` to be used in module
`A`. A possible solution would be to take the exact same approach of
the "current status-quo" seen above for the case of non-parametric
modules: only allow a single definition for each identifier. Under
such approach, the compiler would enforce that a single instance of
a parametric module be used in a program (that is, it would accept the
previously mentioned scenario of `D(5).fd`, but reject the one with
`D(B.globB).fd` and `D(C.globC).fd`, as it would give rise to
a name-clash on symbol `fd`). To give a more concrete example,
if we consider a big-number library parameterised by the size, we
would not be able to use it to operate on two domains of different
sizes. On the upside, it would be much simpler to implement (e.g. the
compiler won't need to manage the cloning and renaming needed to
support multiple instances of the same parametric definition...).

If we consider that allowing a single definition of each symbol is too
restrictive, then we need to devise a mechanism to allow access to
multiple definitions on the same symbol. One simple solution would be
to ask the programmer to write the full-identifier for the intended
symbol. But that would be clearly inconvenient -- notice that,
returning to the example presented above, module `A` accesses `fd`
through intermediate modules, thus "hiding" the constituents of the
full identifier for `fd`.
A more conveniente (and standard) approach is to introduce
**qualifiers** to disambiguate between different objects. When
importing a module. A module **can** be assigned to a qualifier that would
give access to the symbols defined on it (possibly, in sub-modules).

```
// MODULE D(P)
fn fd;
...
---
// MODULE B
...
require D(P1)
...
---
// MODULE C
...
require D(P2)
...
...
// MODULE A
require B as QB
require C as QC
...(uses QB.fd and QC.fd)  // qualifier is used to disambiguate between D(P1).fd or D(P2).fd
```

**Remark:** The use of different qualified objects (e.g. `QB.fd` and
`QC.fd` on the example above) do not necessarily means accessing to
different objects -- they might end up being identified (e.g. if `B.P1`
equals `C.P2`).

**Remark:** In order to retain compatibility with (standard) Jasmin
compiler, we drop the qualifier portion of *full-identifiers* for objects
defined in non-parametric modules (otherwise, we would need to embed the
module identifier in symbols defined on it, as we need to do with parametrised
modules). Of course, that means that qualified imports of non-parametric modules
are useless, as name-clashes won't be avoided.
<!--
**Remark:** In order to preserve compatibility with (standard) Jasmin
compiler, we restrict the use of qualifiers to parametric
modules. Otherwise, we might need to embed a module identifier in the
symbols of a given module (as we need to do with parametrised
modules). Another way of saying it is to state that we identify
full-identifiers of symbols defined on non-parametric modules with
their non-qualified form. Of course, that means that name-clashes on
non-parametric modules won't be avoidable
-->


# Processing phases for the prototype (sketch)

 1. Dependency Analysis
 2. 'Demodularisation' transformation

## Current status

Only patched the parser to support the introduced constructs, and
corresponding command-line options.

repo: https://github.com/bacelar/jasmin/ (branch `deploy-modular`) --
unfortunately with no progress since mid-November (and, sadly, with
little chance of taking on the task before March's retreat).



## Processing phase (sketch)

We constrain dependencies between modules to form a DAG (i.e. we
disallow circular or recursive dependencies).

The processing is splitted in three passes:

 1. Pass I -- Construct the dependency graph;
 2. Pass II -- Consistency check and turn all names qualified;
 3. Pass III -- inline parametric modules;

Extraction into EC (`-mEC` option) only performs the first two passes -- that is, extraction occurs at the output of pass II. Up to this stage, the toplevel module is allowed to be parametric.
For Pass III we further imporse that the toplevel module is non-parametric (otherwise, there would remain unresolved parameters).




For each module (accessed in topological order), collect:
 1. `module identifier`
 2. module parameter signature (name, kind, type)
 3. collect full set of dependencies (mod_qual, mod_name, reduced parameter instances)
 4. full set of defined symbols (including from dependencies)
 5. expanded code

Error checks performed in the pass:
 1. exclude cycles on module dependencies (during preload of import-graph)
 2. detect conflicting names on imports (during processing require clauses)
 3. expand identifiers into their full-identifiers (during processing of module's code)

  
