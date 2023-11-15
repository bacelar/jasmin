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
    import (`as <name>`) and the instantiation of module parameters
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

Each module (parametric or not) is assumed to be interpreted in a
empty context. This is a departure from "plain" Jasmin programs,
where `require` clauses could be used to import modules depending on
some previous declarations [in fact, a typical design-pattern used to
emulate parametric modules in plain Jasmin...].

Of course, this means that `jasminc -mjazz` **is not** backwards
compatible with plain `jasminc`. On the other hand, the whole point of
designing modules is to take a more structured approach for those
awkward code patterns that are often difficult to reason about.

# Fully-qualified identifiers

## Diamond imports and name-clashes

In plain Jasmin, complex import chains are allowed. However, these are
unproblematic because multiple instances of the same symbol can be
safely merged. As an example, consider the following diamond-shaped
sequence of imports:

```
//D:
fn fd(...){...};
//B:
require D
//C:
require D
//A:
require B
require C
...uses fd // refers to B.D.fd or C.D.fd???
```

The question of whether `fd` symbol is accessed through module `B` or
`C` (via `D`) really doesn't matter -- they end up being the same
objects (a single assembly definition). We can say that the **full
identifier** for `fd` in the context of module `A` is indeed `D.fd` --
the distinct import paths leading to its definition only adds
redundancy to the "visibility" of the symbol `fd` from the perspective
of module `A`.

It is intructive at this point to look at a close issue related to
*name-clashing*. If two modules (say `A` and `B`) define some `fX`,
the compiler would emit an error (name clash of `fX`). This is solely
a limitation of the scoping rules adopted by the syntax of the
language, that could be handled by some name-spacing mechanism -- but
"semantically" there would be no problem on allowing to access both
`A.fX` and `B.fX` (here distinguished by what we have called their
*full identifiers*).

## Turning to Parametric-modules

When we consider parametric modules, in an example such as the above
diamond-shaped imports, we might be talking of two distinct instances
of module `D`!!!

```
//D(P):
fn fd;
//B:
require D(P1)
//C:
require D(P2)
//A:
require B
require C
fd... // ??? "fd" refers to B.D(P1).fd or C.D(P2).fd ???
```

We need to decide whether `B.P1` is equal to `C.P2`, and things get much
more intricate when we allow `A, B, C` to be parametric modules
themselves (in which case, we might be considering different instances of
`B.P1` and `C.P2`, since they might depend on own-module
parameters). What we can say is, in the context of some toplevel
(non-parametric) module such as `A`, we can assign a *full identifier*
to all definitions possibly used by `A`. We shall extend the
previously introduced notion of *full
identifiers* for parametric
modulus such as `D` to the shape `D(p1...pn).fd`, where
`p1..pn` are **resolved parameters**. *Resolved parameters* are
defined according to the structure of the parameters:
 - resolved `param int` parameters are integer literals;
 - resolved `global`parameters and `fn` parameters are *full
   identifiers*.
   
In the previous example, full indentifiers could be things like
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
support multiple instances of some definition...).

If we consider that allowing a single definition of each symbol is too
restrictive, then we need to devise a mechanism to allow access to
multiple definitions on the same symbol. One simple solution would be
to ask the programmer to write the full-identifier for the intended
symbol. But that would be clearly inconvenient -- notice that,
returning to the example presented above, module `A` accesses `fd`
through intermediate modules, thus "hiding" the constituents of the
full identifier for `fd`.

## Use of qualifiers to disambiguate different instantiations:

A more conveniente (and standard) approach is to introduce
**qualifiers** to disambiguate between different objects. When
importing a module, it **can** be assigned to a qualifier that would
give access to the symbols defined on it.

```
//D(P):
fn fd;
//B:
require D(P1)
//C:
require D(P2)
//A:
require B as QB
require C as QC
QB.fd or QC.fd  // qualifier is used to disambiguate between D(P1).fd or D(P2).fd
```

**Remark:** The use of different qualified objects (e.g. `QB.fd` and
`QC.fd` on the example above) do not necessarily means accessing to
different objects -- they might end up being identified (e.g. if `P1`
equals `P2`).


## XXX ??? Qualifiers as restrictions

We have seen the use of qualifiers to disambiguate different instances
of parametric modules, but when writing code is often simpler to
annotate where some instances are expected to be the same (and let the
compiler enforce those identifications). Here we move to a reading of
"qualifiers-as-restrictions", and the idea is that qualifiers are seen
and *global* restrictions to parameters (two uses of the same
qualifier in different parts of the program would enforce the
corresponding parameters to match).

```
//A(P):
//B(P):
require A(B_AP(P)) as QA
require C(B_CP(P))
//C(P):
require A(C_AP(P)) as QA
...
//D:
require A(D_AP) as QA
require B(D_BP1) as QB1
require B(D_BP2) as QB2
fA // A(D_AP).fA  --  shared between A, B1 and B2
QB1.fc // C(B_CP(D_BP1)).fc
QB2.fc // C(B_CP(D_BP2)).fc
```

**Remark:** without using `QA` should still work (as, in the end, all
the instantiations resolve to a single one). But it would be less
clear for the programmer; make error messages harder to interpret and
debug; etc.

**Remark2:** a second reason for using restricting qualifiers is
definitely more mundane -- when looking to the meaning of the
parametric module `B`, the use of qualifiers would make it explicit
that a single instance of module A is accessed both by `B` and
`C`. Thus, the EC extraction mechanism would be able to generate a
model that would be simpler and easier to reason about.

## XXX ??? Qualifiers to overcome (some kinds of) name-clashes 

```
//A:
fx
//B:
fx
//C:
require A as QA
require B as QB
QA.fx // refers to A.fx
QB.fx // refers to B.fx
```

but would still not allow:
```
//A:
fx
//B:
require A as QA
fx
fx // ??? refers to fx or A.fx ???
QA.fx // refers to QA.fx
```
It would probably be very simple to overcome, but does not to seems to
worth the effort -- someone writing a program that uses `A` could
easily rename its own def. `fx` -- in the former example `A` and `B`
might be certified third-party libraries defining some usual name (e.g. 'enc')
-- it would have a much bigger impact to get work around such a name clash...

# XXX REST...

In order to simplify the handling of these issues, we consider the use
of *parameter-keys* provided by the user that would act as a global
constraint-system.

```
//A:
require B
require C
fd... // It enforces B.QD=B.P1 and C.QD=C.P2 to be the same
      // hence, both B.D(P1).fd and C.D(P2).fd refer to the same def.
//B:
require D with QD=P1
//C:
require D with QD=P2
//D:
fn fd;
```

In general, the problem is slightly more complex, as parameters can be
involved in intermediate modules...

```
//A(AP):
require B with { PB(AP) }
require C with { PC(AP) }
fd... // It enforces B.QD=C.QD, that is B.P1(PB(AP)) and C.P2(PC(AP)) are the same
      // hence, both B.D(P1).fd and C.D(P2).fd refer to the same def.
//B(BP):
require D with QD = {P1(BP)}
//C(CP):
require D with QD = {P2(CP)}
//D:
fn fd;
```

Here, we might allow "fd" to be used without qualifier, as the only
visible instance is "QD.fd" -- but it should be stressed that defs. in
module D are indeed be prefixed by "QD.". To illustrate this,
consider that `A(AP)` also includes `require D with QDX={P3(AP)}` --
now, `fd` needs to be prefixed by either `QD.` or `QDX.`, and the
plain Jasmin code would be duplicated with prefixes `QD.` and `QDX.`.

**Remark:** I wonder if it would be feasible to do checks on these
possibly duplicated instances (e.g. to emit a warning...)




```
//A:
require B
fn fa
//B:
require C
fn fb
//C:
fn fc
//D:
require A 
require C 
fc << A.B.fc = C.fc (same definition!)
```

```
//A(PA):
require B // QCtxt: C:{QC=B.P1()}
//B:
require C with QC=P1  // QCtxt: C:{QC=B.P1}
//C(PC):
fn fc
//D:
require A with QA=(PA1) // QCtxt: A:{QA} B:() C:{QC=B.P1}
require C with QC=P2 // QCtxt: QA? QB? QC (enforced QC(QA)=QC <=> B.P1=D.P2)
fc << QA.QB?.QC.fc <?> QC.fc (they are the same, because P1=P2)

se em vez disso fosse:
require C with QCX=P2 // QCtxt: QA? QB? QC QCX 
fc << QA?.QB?.QC.fc <?> QCX.fc (not assumed to be the same - P1 and P2 are
not constrained...)

contexts for the modules:
C: QC=B.P1; QCX=D.P2
D: QA=D.PA1; QB?=(); QC=QA.QB?.P1; QCX:D.P2

```


# Import of parametric modules

Parametric modules **should** always be imported instantiating the
complete set of parameters (a complete `with` clause), as specified in
the `modsignature` declaration of the imported module (but possibly
with own parameters...). It **can** also qualify the import, but an
unqualified import would be treated as XXXXX (own name).

```
//A:
require "B" with BN = {bbb} // BN=B(bbb)
require "M" with MP = {ppp} // MP=M(ppp) -- enforce bbb=mbbb(ppp)
require "M" with MQ = {qqq} // MQ=M(qqq) -- enforce bbb=mbbb(qqq)
MP.F.fx vs MQ.F.fx (obs: mas eu queria MP.fx vs. MQ.fx...)
MP.exp vs MQ.exp ( na source, qualificadores podem ser evitados quando
existe um único módulo que "ofereça" def.
//B (bbb):
require "U". // BU=U()  -- qual. will be ignored upon extraction...
//M (ppp or qqq):
require "B" with BN = {mbbb} // BN=B(mbbb) 
require "F" with {fff} // enforce mbbb=fbbb(fff) ={BN}
//R:
require "B" with {rbbb} // NÃO PODE SER (perde rastro do qualifier...)
require "B" with BN = {rbbb}  // DEFINE key PARA RESTRIÇÕES
//F(fparams):
require "B" with BN = {fb} // 
require "R" with BN = {fr} // ?enforce fb(fparams)=fr(fparams)  ??PORQUÊ???
require "E" with ? = {eee}
//E:
require "B" with BN = {...}
fn exp
```

```
//A:
require "B" as "BN" with {bbb} // BN=B(bbb)
require "M" as "MP" with {ppp} // MP=M(ppp) -- enforce bbb=mbbb(ppp)
require "M" as "MQ" with {qqq} // MQ=M(qqq) -- enforce bbb=mbbb(qqq)
//B (bbb):
require "U" as "BU". // BU=U()  -- qual. will be ignored upon extraction...
//M (ppp or qqq):
require "B" as "BN" with {mbbb} // BN=B(mbbb) 
require "F" with {fff} // enforce mbbb=fbbb(fff)
//R:
require "B" with {rbbb}
//F:
require "B" as "BN" with {fbbb} // 
require "R" as "BN" with {frrr} // ?enforce fbbb=rbbb(frrr)  ??PORQUÊ???
require "E" with {eee}
```

Critically, qualifiers in parametric imports would be
linked to the contents of the `with` clause. That is, qualifiers could
be reused (in different import clauses), but each qualifier should
be in correspondence with the parameter instantiations.



# Qualifiers in `require` clause

A `require` clause **can** add a qualifier (with `as <qual>`) when
importing both parametric (with `with` clause) and non-parametric
modules. But it should be noted that the primary intent of qualifiers
is to specify how we access declarations of the imported module on the
source program, and not directly related to "identity" of those
declarations. This is specifically relevant when importing
non-parametric modules. Consider the following scenario:

```
// module A
u64 a = 33;
require "B" as "QB"
...
// module B
u64 a = 22;
...
```
the compiler **should** emit a "name clash" error. The rational being
that the name `a` is taken by module `A`, and qualifier `QB` only
makes the symbols in `B` accessible as `QB.*` in the source code --
does not duplicate them...

**Remark:** The last restriction could be avoided if one considers a
unique *module identifier* for each (non-parametric) module. It could
be either implicit (e.g. filename) or given in the module preamble
(e.g. `moduleId "XPTO"`) -- that *module identifier* would prefix
every name defined on the module, and hence would avoid the
aforementioned name-clash (there, qualifiers would allow to
disambiguate which definition is intended...).

```
// module A
require "B"
require "C" as "QC" /* OK, but with two clones of "C" ??? */
...
// module B
require "C"
```



# Name prefixing and name clashing


Parametric modules can be instantiated multiple times. That means that
declarations in those module instances (constants/functions) need
to be compiled into different objects. We shall use prefixes with the
*qualifier path* from the innermost non-parametric module.

- Declarations inside a parametric module **shall** be prefixed with
  the corresponding *qualifier path*;
  
