# TODOs for POO

In the future, we may add the following features and optimizations.

## Meta-Object Protocol

Redefine POO using
[a compositional MOP for Prototype Inheritance](https://github.com/fare/projects/issues/7),
i.e. a Meta-Object Protocol in the spirit of the CLOS MOP.

### Sources of Inspiration

Get inspired by the specification of [AMOP](https://cliki.net/AMOP), except without classes.
As reference implementation code, see for instance
[Closette](https://github.com/binghe/closette/blob/master/closette.lisp)

Start from something minimal, in the spirit of
[POP](https://github.com/divnix/POP/blob/main/POP.md),
and/or of these two functions:
```
(def (instantiate p b) (def f (p (λ a (apply f a)) b)) f)
(def ((inherit p q) f s) (p f (q f s)))
```
Or instead of having an eta-converted function `(λ a (apply f a))` we could use lazy evaluation:
```
(def (lazy-instantiate p b) (def f (lazy (p f b))) f)
```

### A MOP for inheritance

Primitives that we may want to intercept include:
  - method computation from some point in the hierarchy (`compute-slot`),
  - linearization the inheritance DAG (`compute-precedence-list`),
  - etc.

Also get inspired by Racket-style proxies and impersonators so even basic types
can carry prototype information, and be modified or instrumented through prototype mechanisms,
e.g. to trace all arithmetic operations in parts of a program.

Allow user-defined storage strategies, so the storage optimizations described below
could be done purely "in userland".

For instance, allow users to select which optimizations and constraints their object hierarchies use,
e.g. single-inheritance, static strong typing with a given system, etc.

Allow handling of undefined methods with a mechanism akin to
CLOS's `no-applicable-method` or smalltalk's `doesNotUnderstand`.

Use that mechanism to implement "lazy delegation" like
[Fisher's and Shivers' Ziggurat](https://www.ccs.neu.edu/home/shivers/papers/ziggurat-jfp.pdf),
by inheriting from `(fallback-to ,lazy-expr)` or something,
which implements the `no-applicable-method` protocol by
having it delegate messages to some `.fallback-handler` field lazily initialized
with the provided expression.

### MOP for method-combination

Allow for compositional definition of method-combination, and more generally of non-record objects:
effectful procedures (or, with a tweak, monadic functions) can be defined from many fragments
(primary, before, after, around methods, etc.).

But values of other kinds of types can also be made of many fragments.
Indeed, records of values in as many "slots", i.e. "records", "structures", "objects",
are the prime example of something being defined from many fragments (or slots, fields, components, etc.).
Surely, these two kinds of computations follow a common pattern, and
should be defined according to a same set of macros, as well as further such incremental definitions?

Actually, just like method qualifiers or object slots are a way to incrementally define part of an object,
method specialization is also a way to incrementally define part of a function.
More generally, this is all about incrementally defining part of "the universe".
How do you divide the universe into parts and reconstruct it from it,
and further divide these parts into subparts, etc.?
There should be a general mechanism to define a program recursively
in terms of such composable incremental fragments.
(Bonus: then, how do you deal with "degeneracy" in the sense of Gerald J. Sussman?)

Primitives that we may want to intercept include:
  - partial specification of a fragment of an object or sub-object
    (see in CLOS, `(defmethod foo :qualifier ((x specializer) ...) ...)` or in Nix, `a.b.c = d;`).
  - computation of the function / object from the fragments.
  - general inlining to skip over from chasing the sub-component back from the complete universe
    to directly computing with the sub-component without computing the whole universe,
    i.e. instead of `universe.a.b.c` directly deal with `b.c`;
    instead of computing the entire `generic function`, deal with the "component" that handles
    the arguments at hand, only.
  - method lookup (`.ref`), e.g. for debugging, transactional persistence or access control,
    or just for reconstituting the sub-component of an object from its fragments.
  - method side-effects (`.putslot!` and `.put!` or whatever replaces them), for same reasons,
    and not the "reverse" operation of setting the fragments right from the composed value, too.

### Design and implementation of the MOP

As in POP, start from distinct and general notions of prototype and instance
(indeed, the most type-parametric notion of prototype function).
Then, combine them into a notion of object, and
add the MOP as another layer of fixed point to the design.
Indeed, experiments in POP to add multiple inheritance to prototype objects
suggest that the prototype function, direct and effective super class lists, etc.,
and other meta-information, should be be grouped together in a single entity
that generalizes the prototype as a function strictly speaking.
I suppose that in the general case, this entity that contains all this information,
is what gets to be called "prototype", whereas the function from the simple case,
now only a component of the prototype, gets a different name, like "prototype function".
In any case, this "prototype" entity is itself subject to being computed
as the instance of a prototype fixed-point. And since it has several components,
it can be organized as a combined instance-and-prototype object rather
than an instance detached from its prototype, hence the MOP bootstrapping.

Maybe the meta-object may be a record that caches together
both the prototype function and its lazy fixed point,
together with a meta-object, itself a prototype that describes how instantiation and composition work,
as well as additional type-specific accessors or extension points, encoding information, etc.
```
(defstruct poo (meta prototype instance) constructor: :init!)
```

### More efficient internal object representation

The "prototype" is itself an object, that specifies more than simply a function (see above).

*If* semantically the instance type is a table from resolved-identifiers to values,
rather than symbol to values,
then maybe the "prototype" field is such a value with
a special identifier `prototype` defined in the MOP.
In the meantime, we just reserve a special symbol, e.g. `.prototype`.

With a separate meta-object, we can compositionally build up the meta-object
to refine how the prototypes are instantiated and composed, how they can be built in terms
of "extension points" each of which will be its own prototype, etc.
With "objects", an extension point is further refined to specify a record of extension points;
with "method combinations", some extension point will control the flattening of individual prototypes;
with "prototype linearization", super prototypes are a dependency graph, not a mere list,
and a list is first extracted from that graph (based on some external global well-ordering
of all prototypes?).
The "object" reference would provide context for the lazy evaluation of whatever "attributes"
(each the cached computed fixed-point value of some "extension point"), etc.

## Syntax

Maybe improve the object definition syntax using keywords, as in e.g.
`(.o self: self super: super inherit: [supers ...] bind: (x y z) bind-these: #t (slot forms) ...)`
To implement it with `syntax-case`, see e.g. these definitions for [defproto](https://github.com/vyzo/gerbil/blob/ee22de628a656ee59c6c72bc25d7b2e25a4ece2f/src/std/actor/proto.ss#L261) and [defhandler](https://github.com/belmarca/gerbil-fwd/blob/83120eac03fa39338c82993d3041ddad01432419/fwd/routing.ss#L65).

Make it optional whether to include the currently-defined slots in the list of slots to be bound.

## Simple Features

Constraint-checking assertions and other instantiation-time side-effects
as part of the `.instantiate` function.

## Classes

Our `mop.ss`, `type.ss`, `number.ss`, already have large chunks of it, but
we could and should probably better support class-based object orientation
using POO as the object system used at its meta-level to represent classes.

The same descriptor meta-object, viewed as a prototype is a class descriptor,
and viewed as an instance is a type descriptor.

Its element template prototype can specify provide default slot values
as well as constraints on slot types and slot values.

A special slot in each object contains the type-descriptor
(it is `.type` in our current setup; it semantically analogous to the `vtable` in C++,
though the current implementation is less efficient,
though it might be simpler start to experiment with meta-object protocols).

  * Constraint-checking assertions and other instantiation-time side-effects,
    and a function `.instantiate` to invoke them without accessing a slot.

  * Enforcement of a discipline on prototype mutability.
    Objects must not be modified after having been used as super-prototypes.

  * Reflection on objects, to view a list of slots, to intercept how slots are computed,
    or determine who has access to which slots when.

  * Support for conditionally-defined slots, or slots for computed names.

  * Support for slot definition macros with non-local effects to the rest of the object,
    e.g. to declare a slot as "public" or "private",
    and have that reflected in a list of either kind of slots, and/or in slot-property meta-slot;
    similarly, slot combination methods as meta-information, similar to method combination in CLOS.

  * A better implementation of Jsonnet and/or Nix in Gerbil, based on POO (?)

  * Identify and implement the "usual" optimizations that e.g. SBCL uses for CLOS.

## Data representation

The use of mutable hash-table's is a poor man's solution to organizing data.
For non-mutable use, represent prototypes as pure persistent maps instead.

Make sure these maps are merged in a way that maximizes sharing of state.
Maybe even with hash-consing.

For performance, have an optimized representation for tables with few entries
(under 4? 8? 16?), e.g. a flat vector with a linear search.
For extra performance, maybe automatically keep it sorted by access frequency?

Specially represent prototype methods that ignore their super-method in
an efficient way such that multiple successive overrides of a same method
won't create an indefinitely growing chain of forgotten values.
For instance, one linked list of methods per slot name, or instead of a list,
"just" composing functions including "lossy" ones like the K combinator.

Represent the instance (or its base layer, if there are many layers) as a vector,
wherein the indexes are based on the hash-consed "shape" of the prototype,
the shape being the sorted list of its slot names.

Maybe intern the symbols used as prototype slot names into a table assigning each a unique integer
(at load time? or based on tree walking the entire program?), and use some hash-consed word-granular
(rather than bitwise) patricia tree data structure for shapes.

For the mutable case, a tree might work too, though a hash-table is simpler.
For a real efficient use, we would like an array containing the values,
but then comes the question of the mapping from slot name to array index,
especially in presence of multiple-inheritance, where it is not enough to know
a constant super-object to assume a constant array index.

The mapping from slot-name to array index given object can be refactored to
mapping given "shape", when the shape forgets other irrelevant details about the object
and can share data between objects of same shape (e.g. variants of a same super-object).

This mapping is typically itself a immutable tree.

For higher performance, we can have cache the mapping from object and slot to array index,
taking advantage of common cases where the slot is constant, and/or the shape is constant
or varies little. Possibly, each access site could have its own inline cache, as in SBCL.

Beware that proper detection of the optimizable cases (e.g. constant shape) is non-trivial,
that caching all the time is costly, and that JITing to do it only where it matters is complex.

Make sure this works well with constant-folding in the underlying compiler.

A Meta-Unquote Protocol (MUP) could provide at the same time reader/printer information,
mapping between "virtual" structure (accessor function) and "real" structure (physical layout),
an object structure description that enables runtime as well as compile-time optimizations.

## Better Debugging

Detect circular definitions with a suitable variant of hash-ensure-ref,
and report them nicely.

In Slate a mixin is an object with behavior and no state.
In Squeak, a Trait has a protocol that has modular requirements and provisions.
Records, delegation. Object/Meta delegation flag attached to the slot.
e.g. for "new", you want a new object, not a new class.

Multimethod dispatch: the earlier arguments would take priority over later one for method-not-found;
    in DSLs, the method-not-found would do autodiscovery from e.g. the filesystem.
    Subjective dispatch vs objective dispatch.
    Performance: Method inline caching.

  * Look what we can save from [TinyCLOS](https://github.com/ultraschemer/gambit-tiny-clos/blob/master/tiny-clos/core.scm) ?

##

  * Design and implement a type system that works well with POO.
    https://github.com/fare/projects/issues/3
    This type system probably would have some notion of subtyping, such that
    a function prototype has type `(forall a (forall b < a (b <- (b <- a) <- a)))`.
* [A good type system for prototype inheritance](https://github.com/fare/projects/issues/3)


## Notes

We have to start with a gambit: we pay a price for our choice of representation,
so we can gain the advantage of uniform reasoning and optimizations later. Possibilities:
- Make everything a lazy value, and directly use the same formulas as in Nix.
- Make everything a closure, and be more like the original Yale T object system.
- Make everything a hash-table, and be more like JavaScript objects(?).
- Make everything a struct, and be more like C++ objects.
- Add a hidden "meta" field to everything, and be more like Clojure objects.

Since we're a Scheme, we'll opt for everything is a closure.
How then do we represent magic meta-information?
- A call with a special magic argument?
  Works well if e.g. usual first argument is a symbol, but this one isn't.
- A global weak hash-table keyed by the closures? Rather slow.
- Some implementation-specific extension field. Not portable, and not available on Gerbil.
Normal: ← ⇐ Long: ⟵ ⇐

Possible representations for objects of schema A_ (fun from Symbol to Type):
- (Fun (A_ msg) <- msg:Symbol)
  Good: very simple. Bad: makes introspection hard, or must be separate,
  or the function also accepts special magic input parameters.
- (Fun (PureTable|Table (Fun|Lazy (A_ msg)) <- msg:Symbol))
  Basically, break down the previous into a table, per slot.
  Good: "functorially" maps the structure of prototype onto that of desired
  values.
- (Fun (Fun|Table|PureTable SlotInfo <- Symbol))
  An object computes a table from symbol to SlotInfo.
  Here, SlotInfo would be as follows:
(SlotInfo A B) = (Record ;; should this "just" be a (Meta A B) object?
  ;; or maybe we need a lens-like (Meta S T A B) object?
  instance-value: [(Maybe A)]
  flags: [Nat] ;; 1: cached? 2: default? 4: ignores-self? 8: ignores-super?
  direct-proto-fun: [(Fun A <- A D)] ;; meta
  parents-proto-superfun: [(Fun D <- B)] ;; computed during instantiation
  direct-default-value: [(Maybe B)])

In practice, using (case ...) make should implementation relatively efficient
for plenty of keys at once.

### Old internals

Internally, in the current implementation, an object, or poo, is `Poo` struct, with two slots:
a list of elementary prototypes and an instance.

Each elementary prototype is a hash-table mapping each defined slot name to a function
that computes the slot value from two arguments:
  1. a reference to the object itself,
  2. the list of super-prototypes.

The instance is a hash-table mapping for each slot name the value computed
by using the prototypes, or otherwise explicitly set as a side-effectful override.

Note that this model is not capable of supporting a slightly more expressive object model
where computations can access arbitrary slots of the super-object,
or a reified version of the super-object itself.
If such an extension is considered useful, it may be implemented by resurrecting
a notion of "layers" present in a previous version of the code, wherein each instance
contains a list of layers, one for each prototype in the list, that maps slot names to values
for the definitions present in that given prototype.
The first layer can also serve to cache all slot computations and
hold values of slots modified by side-effects.

## Generic Data Structure Library

On top of Gerbil-POO, build an
[Efficient, Incremental, Generic, Isomorphism-aware, Correct Data Structures](https://github.com/fare/projects/issues/10)

## Property-based testing

Use type descriptors as the basis for a property-based testing library,
in the style of
[Hypothesis](https://github.com/HypothesisWorks/hypothesis/tree/master/hypothesis-python),
[QuickCheck (Haskell)](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf),
[QuickCheck (Racket)](https://docs.racket-lang.org/quickcheck/index.html),
[rackcheck (Racket)](https://docs.racket-lang.org/rackcheck/index.html),
[guile-quickcheck (Guile)](https://ngyro.com/software/guile-quickcheck.html),
[test-generative (Chicken)](https://wiki.call-cc.org/eggref/5/test-generative),
[check-it (Common Lisp)](https://github.com/DalekBaldwin/check-it)...

Other type descriptors
[GNU Poke](http://www.jemarch.net/poke)
