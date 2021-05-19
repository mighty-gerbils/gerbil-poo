# Prototype Object Orientation in Gerbil Scheme

This directory implements POO, a system for Prototype Object Orientation,
with a pure lazy functional interface, as well as multiple-inheritance.

## Conceptual Overview of POO

### Prototypes: the Essence of Object Orientation

In POO, an *object*, embodies two related but very different concepts,
that it is important to distinguish: a *prototype*, and an *instance*.

*Prototypes* are open recursion schemes used to represent the partial,
incremental specification of computations.
You can *compose* prototypes together to piece those
partial specifications into more complete specifications,
according to a process called *inheritance*.

Once you have reached a specification to your liking
by composing smaller prototypes into bigger prototypes,
you can extract the according computation and compute its result
by *instantiating* the prototype of your choice,
using a simple *fixed-point* operator, yielding an *instance*.

In a prototype object language, these prototypes,
their composition and their instantiation, are activities that take place
*at runtime*, amidst arbitrary computations, and not just *at compile-time*
with onerous restrictions as is the case in most "OO" languages (see below).

### Instances

_Typically_, an *instance* is shaped as a set of *slots*,
each binding a *name* to a *value*.

(Note: in other languages, *slots* may instead be called "fields" or "attributes";
or, when the associated values are functions, the slots, or their values,
are often called "methods" or "members" (or "member functions") of the instance.)

A *prototype* is an incremental description of how each slot of an instance
can be computed from (a) the other slots of the instance,
and (b) slot computations *inherited* from some super-prototypes.
The inherited slot computations can recursively refer to further inherited slot computations;
while more slots may refer to each other.
When *instantiating* a prototype into an instance,
the resulting instance is the fixed point of all these computations.

The slot names are usually strings, symbols (interned strings) in symbolic languages,
or sometimes identifiers (compile-time entities, often elided at runtime for performance).
The values are arbitrary, but their may be formal or informal constraints
for the values associated to a given slot to be restricted to a type associated to the slot.

Thus, in this common case, the *instances* are some kind of "records" or "dictionaries"
—indeed often class or typeclass descriptors (see below).

However, in the most general case, the instances of a prototype need not be
"records", "products", "dictionaries", or "class descriptors",
but can be values of arbitrary types:
numbers, functions, pictures, music, processes, documents, configuration files, etc.

Still, having all (or at least common) instances be of some dedicated kind,
enables a very nice feature in prototype object languages:
conflating instance and prototype in a single entity, the "object".
Obviously this is not possible when the instance values are numbers:
numbers have no memory of the computation that led to them.

### Conflating Instance and Prototype

In a pure functional setting, where there can be no side-effect,
all instances of a prototype are necessarily equal:
all slot computations will necessarily will yield the same results,
no matter how many times they are attempted.

Thus, you can cache the instance with the prototype — or, seen another way,
you can carry with every instance the prototype from which it was generated
(and it wasn't explicitly computed by instantiating a prototype, that's
equivalent to having been instantiated by a prototype that yields a constant).
Thus, it is meaningful in a pure functional setting (or mostly pure one)
to manipulate together prototype and instance as a single entity, the *object*;
and it is both meaningful and practical to do so in a way that
the two can be separated whenever needed:

  - You can use the object's prototype to further build new prototypes,
    and the associated instances and objects, at which point you ignore
    or discard the instance information from the object.

  - You can shallowly copy the object's instance or use and export its
    computation results, at which point you ignore or discard the prototype
    information from the object.

  - Or, what is very interesting, you can manipulate the object without
    having to know or decide in advance when the information is complete
    or when it isn't, for even "complete" they can be further composed,
    refined, overridden, inherited from, extended, restricted, decomposed,
    etc., to build further objects, themselves extensible and manipulatable.

  - This ability to copy the parts of a computation you like, disassemble
    and reassemble them differently, removing some parts and adding new ones,
    etc., make prototype object programming extremely useful for rapid
    configuration changes to interactively react to new ideas and circumstances.

### Inheritance

When you compose prototypes together, the information specified in
earlier/leftmost/child/sub- prototypes takes *precedence* over
the information from the latter/rightmost/parent/super- prototypes.
We also say that the former prototypes *override* the latter,
and that the values the former compute may *inherit*
from the values the latter compute, or *delegate* to them.
(Beware: in Nix, some of the tradition "extension" functions,
which essentially equivalent to the above, reverse left and right
in this precedence order).

When combining prototypes together, we call *inheritance* the structure
resulting from their child/parent sub/super relationships.

#### Single Inheritance

The simplest way to combine prototypes is by "single inheritance":
each prototype may inherit from one direct super prototype, that
may inherit from one direct super prototype, etc.
The inheritance relationship between prototypes is thus a total order.
The inheritance data structure is a list.
To instantiate, you compose all the prototypes in the list,
and take the fixed-point.

Single inheritance is simple to implement, has a few easy optimizations,
and is often used at first in object systems.
The first version of POO used single-inheritance, as do Jsonnet
and the Nix "extension systems" that inspired its design.
But single inheritance has drawbacks: while you can keep "mixin" prototypes
that you may carefully insert in your inheritance hierarchies,
you cannot automatically combine a mixin with those it depends on,
or, when many mixins depend on super-mixin, multiple copies of that super-mixin
will be inserted in your prototype list, sometimes out-of-order, in a way
that one mixin duplicates or overrides the behavior of the other.
Soon, you end up manually curating complex dependencies between mixins,
and this management doesn't scale when you start to use libraries.

#### Multiple Inheritance

A more elaborate solution is multiple inheritance, wherein
a prototype may declare several other prototypes
as direct "super" prototypes that it depends on.
The inheritance relationship is a partial order;
the inheritance structure is a directed acyclic graph (a DAG).
Users may thus express the fine dependencies between their mixins
without causing some mixins to be inserted multiple times and/or out of order.

Still, to compose your prototypes, you need a list, a total order,
such that you can chain your partial computations from left to right.
Therefore, to instantiate a prototype, you must first *linearize*
its inheritance DAG into a *precedence list*,
a total order that completes the DAG, of which the DAG is subset.
There are many ways to linearize it, but modern languages use
the [C3 linearization algorithm](https://en.wikipedia.org/wiki/C3_linearization),
initially introduced in Dylan,
for its nice properties: notably, it ensures that the precedence list
of a super-prototype is always a sub-list (not necessarily contiguously)
of the precedence list of its sub-prototypes.

### Prototypes Easily Generalize Classes

In "class-based" languages, there are two separate stages of computation,
each of them limited in expressiveness. In the earlier stage, at compile-time,
the instances are classes or class-descriptors, and very few computations are
available beside direct inheritance of class prototypes. The distinction
between class-as-prototype (partial information about a type) and
class-as-instance (a data type) is seldom made explicit,
or the relationship well understood and explained.
In the latter stage, no composition or instantiation of prototypes is allowed anymore,
and though some runtime entities may be called "objects" or "instances",
they are only the elements of the types, with no fixed point involved.

It is trivial to implement classes on top of prototypes:
just design a simple representation for type descriptors as your instances,
and the prototypes for the typese descriptors will naturally be classes.

It is practically infeasible to implement prototypes on top of classes:
all the runtime manipulations you'd like to do are impossible
with the builtin compile-time-only class mechanism;
meanwhile, typical type restrictions associated to classes
will force you to use extremely onerous dynamic-typing-on-top-of-static-typing
encodings to be able to express the arbitrary computations
you may want to do with prototypes, at which point they are too cumbersome to
interoperate with the rest of your program, in addition to being
an order of magnitude too verbose for their own good.

### The Essence of OOP

Prototypes embody the essence of both functional and object-oriented programming.
On top of this solid conceptual foundation, we can easily build traditional
object system features, such as classes, multiple-inheritance, default slot values, etc.

## Historical Background

The semantics of POO is very close the object systems of the
[Nix Expression Language](https://nixos.wiki/wiki/Nix_Expression_Language)
(as defined as a library in a few lines in
[`nixpkgs/lib/fixed-points.nix`](https://github.com/NixOS/nixpkgs/blob/master/lib/fixed-points.nix)),
itself essentially identical to that builtin to [Jsonnet](https://jsonnet.org/).
Other influences of note include the [Slate language](https://github.com/briantrice/slate-language)
and of course the Yale T Scheme object system by Jonathan Rees.

Moreover, we also added multiple inheritance and default arguments in the style of CLOS,
and C3 linearization in the style of Dylan, just like we also did for Nix in
[pop.nix](https://github.com/muknio/nixpkgs/blob/devel/lib/pop.nix)
(see the explanation in [pop.md](https://github.com/muknio/nixpkgs/blob/devel/lib/pop.md)).

Pure lazy functional prototype object systems are ideal to incrementally define such things as:
  * configuration for building, installing, and deploying software on a machine or network of machines
    (as used by Nix, NixOS, DisNix, NixOps,
    but also by Nix or Jsonnet front-ends to terraform or kubernetes),
  * compile-time representation of objects, types and classes inside a compiler,
  * objects with dynamic combinations of traits that are hard to express in class-based systems.
  * rich user interfaces with interactively defined configurations.

Each prototype specifies a list of super-prototypes to directly inherit from.
These direct super-prototypes may themselves depend on other prototypes.
The direct and indirect super-prototypes are organized in a DAG — a directed acyclic graph.
This inheritance graph is reduced to a *precedence list* using a *linearization algorithm*
that yields a total order compatible with the partial order of the DAG.
We use the same "C3 linearization algorithm" as all modern languages for its nice properties:
the precedence list of a prototype always includes as (possibly non-contiguous) sublists
the precedence lists of each of its superclasses, and its direct superclasses are included
as early as possible in the list.

A list of prototypes can be combined into a new prototype by *inheritance*.
_Within the scope of this combination_, each prototype in the list is considered
a *super-prototype* of the prototypes appearing earlier in the list,
and a *direct super-prototype* of the prototype appearing before indirect super-prototypes
immediately before it.
The earlier prototype is said to *inherit from* the super-prototypes,
and to *directly inherit from* its direct super-prototype.
To compute a slot in the combination,
the formula specified by the prototype at the head of the list is used;
if that formula invokes the inherited slot computation, then
the formula for its direct super-prototype is used,
which may in turn invoke the formula from its further direct super-prototype, etc.

If a prototype doesn't specify a slot definition, the result is functionally equivalent to
specifying a slot definition that explicitly just invokes and reuses the inherited slot computation.
If the last prototype in an inheritance list
implicitly or explicitly invokes its inherited slot computation,
then the combined prototype will in turn invoke its direct super-prototype when further combined;
if the prototype is instantiated without further combination, then
an error is raised when a slot is computed that invokes an inherited computation past
the end of the inheritance list.

Note that in the current model, the only way the super-prototype is by invoking
the inherited computation of a slot within the computation of that very same slot:
they are not currently allowed to access a reified "super-instance"
with a notional mapping of slot names to values, or
to access other slots than the current one in that notional mapping.
This allows for notable simplifications in the implementation,
compared to indeed allowing access to such a reified "super-instance",
or to layers of slot bindings, one for each prototype involved in an instance.

In a pure functional lazy setting, all instances of a prototype are the same;
it is appropriate to speak of *the* instance of that prototype, and
to manipulate them together into a single *object*:
when used as part of combinations, the prototype part is used;
when accessing slots, the prototype is implicitly instantiated and the associated instance is used.
A single special form `.o` is used to define an object that simultaneously embodies
a prototype and its instance.

If you use side-effects, it is good discipline (to be enforced in a future version of POO?)
that any prototype used as super-prototype should be immutable
by the time any such combination is instantiated.
To instantiate a prototype `object` multiple times for the purpose of side-effects,
"just" create a new instance with `(.mix object)`.

### Further common concepts

A slot computation that doesn't invoke its inherited computation is said to *override* it.
The simplest slot definition is to specify a constant value as the computation,
which will indeed override any inherited computation.

It is not uncommon for a slot computation to always raise an error:
or to be left undefined, which will also implicitly result in an error if accessed:
a further prototype may override this definition, and the error will only be raised
if that default computation is invoked rather than overridden.

A more advanced slot computation may invoke the inherited value and modify it,
for instance increment a count, add one or many elements to a list or set, etc.
The case is common enough that there is a special syntax for when a slot computation
always invokes its inherited computation and modifies its result.

A *method* is a slot the value of which is a function.
The body of the function may or may not use the values of other slots.
The special form `.call` can be used to directly invoke a method.

A *mixin*, or *trait*, is a prototype specifying a partial increment of computation,
referring to slots that it doesn't define, and/or
providing slots that are meant for further use rather than as final values.
Unlike perhaps in other object systems, in POO there is no
technical difference, syntactic distinction, or special semantic treatment,
between prototypes that are or aren't mixins.

Slot computations aren't evaluated until an prototype is instantiated and
the corresponding slot is accessed using the `.ref` function
(or derivative special forms `.call` and `.get`).
Slot computations are thus *lazy*, and may in turn trigger the lazy computation of further slots.
Internally, the implementation uses the standard module `std/lazy` where appropriate.

When incrementally describing a configuration, often a hierarchy of prototypes is defined.
That's where the dual nature of objects as both prototypes and instances becomes really handy:
an object slot may itself contain an object, and so on, and you don't need to track which is which
to recursively instantiate them — every recursive object will be automatically and lazily instantiated
right at the place it is expected when you print or otherwise use the final configuration object.
Internal objects typically contain references back to the surrounding containing object,
from which they can access configuration for parent and sibling configuration objects;
for instance a server's IP address and configuration port will be propagated for use by clients,
that may in turn offer services for use by further clients.
When a mixin adding a service may incrementally refine recursive configuration objects
for many clients and servers...
Beautiful incremental configuration.

### POO Definition Syntax

You can define a *poo* with the special form `.o`
(where the dot is a marker for special POO syntax, where the "o" stands for "object",
and where the syntax is deliberately kept short),
or the special brace syntax (see below), with the following template:

```
(.o [([:: [self [super [extra-slots ...]]]])] slot-definitions ...)
```

The first list of parameters is optional;
it can be omitted, or it can specified as an empty list `()`
to prevent confusion in case you define a macro the users of which
might want to use the keyword `::` as the name of a slot.
In that list:

  * The optional parameter `self` specifies a symbol that will be bound to
    the *instance* whose values are computed. Note that this instance
    need not be that of the *prototype* being currently defined, but may be
    that of any prototype that inherits from the prototype being defined.

  * The optional parameter `super` specifies a prototype, list of prototypes,
    or recursively nested lists or pairs of prototypes,
    that the prototype being currently defined *directly* inherits from.
    You can specify the empty list `[]` if there are no such super-prototypes.

  * The optional following list of parameters are as many of slot names that
    will be bound to macros to access the relevant slots of the instance being
    computed when the particular slot value is computed.
    Definitions for these slots are presumably to be provided by
    other prototypes in the object being instantiated,
    since those in the current prototype will already be implicitly included
    in the list of symbols to bind.

Each entry in `slot-definitions` specifies how to compute a given named slot:

  1. When the computation `form` wholly ignores the inherited computation, and *overrides* it,
     the entry is simply:
     ```
     (slot-name form)
     ```

  2. When the computation always invokes the inherited computation and passes it
     to a combining function `function-form`, to be optionally followed
     by extra arguments `extra-function-args` in the function call,
     the entry is:
     ```
     (slot-name => function-form extra-function-args ...)
     ```

  3. As a special case of the above, when the function is `.+` which
     overrides the first prototype argument with the second one
     (by appending the first to the supers of the second). The entry is:
     ```
     (slot-name =>.+ overriding-prototype)
     ```

  4. In the more general case that the computation may or may not invoke the inherited computation,
     depending on some condition, then a user-specified symbol will be bound to
     a special form invoking that inherited computation, and the computation `form`
     may use that special form; then the entry is:
     ```
     (slot-name (inherited-computation) form)
     ```

  5. As a short-hand for a common case, a slot may be defined to take the value
     of a same-named variable in the surrounding lexical scope. The entry is simply:
     ```
     (slot-name)
     ```
     or even just
     ```
     slot-name
     ```

  6. You can also define a default value for a slot.
     This value cannot depend on either `self` or `super` arguments;
     instead it serves as base for the recursion, the ultimate `super` argument,
     that will override inherited defaults or be overriden by further defaults.
     The default is computed as the most-overriding of all of them, and *then*
     the usual methods are called. The entry for that is as follows:
     ```
     (slot-name ? default-value)
     ```

  7. As an alternative to a simple `(slot-name form)`, `(slot-name => form)`
     `(slot-name =>.+ form)`, or `(slot-name ? default-value)`,
     you can write as a keyword `slot-name:` followed by the `spec`
     with the optional `=>` or `=>.+` before it.
     ```
     slot-name: form
     slot-name: => form
     slot-name: =>.+ form
     slot-name: ? default-value
     ```

As a short-hand, a new variable may be defined and bound to a prototype object with the form:
```
(.def (name [self] [super] [slots ...]) slot-definitions ...)
```
That form is equivalent to `(def name (.o (:: name super slots ...) slot-definitions ...))`.
The `name` can be a simple symbol in which case the options are omitted.

The special brace syntax is available if you `(import :clan/poo/brace)`
and overrides the `{}` syntax of Gerbil so that `{spec ...}` is the same as `(.o spec ...)`.
Note that this works by overriding the binding of the special symbol `@method`,
and that to define or use Gerbil methods, you will have to use `(@@method x ...)`
where you previously would have used `{x ...}` or `(@method x ...)`.


### POO Usage Syntax

To combine any number of objects or recursive list of objects using inheritance,
use the function `.mix`:
```
(.mix object1 [object2 [object3 object4] object5] object6 [] object7)
```

To refer to a slot in an object, use the function `.ref`:
```
(.ref object 'x)
```

To access a slot named by a constant symbol, use the macro `.get` as short-hand:
```
(.get object x)
```
The macro also works with any constant object for a name;
if multiple names are specified, the names are used in sequence
to recursively access slots of nested objects:
```
(.get object x y z)
```

You can recognize whether a value is a POO object with `object?`
```
(assert-equal! (object? (.o (x 1) (y 2))) #t)
(assert-equal! (object? 42) #f)
```

(The predicate for Gerbil's builtin object type is reexported as `@object?`
and its constructor as `@make-object`.)

A few special forms allow for side-effects.
They break the usual pure functional interface, so use with caution.

1. The `.put!` special form side-effects the value of a slot in an object's *instance*,
   without modifying its *prototype*.
   You can even create slots that have no definition in the prototype:
   ```
   (.def o a: 1)
   (assert-equal! (.@ o a) 1)
   (.put! o 'a 2)
   (.put! o 'b 3)
   (assert-equal! (.@ o a) 2)
   (assert-equal! (.@ o b) 3)
   (def o2 (.mix o))
   (assert-equal! (.@ o2 a) 1)
   ```

2. The `.set!` is like `put!` with a constant, implicitly quoted, slot name:
   ```
   (.def o a: 1)
   (assert-equal! (.@ o a) 1)
   (.set! o a 2)
   (assert-equal! (.@ o a) 2)
   ```

3. The `.putslot!` special form will modify a method definition in the prototype.
   Note that if the object or other objects inheriting from it have already been
   instantiated, this change will not affect these existing instances, and that
   you may have to explicitly call `uninstantiate-object!` on them to flush their
   instances — which will also forget any modification from `put!`.
   Also note that in the current implementation, the cost of this function is linear
   in the number of slots directly defined in the prototype
   (as opposed to defined in one of the prototype's supers).
   The arguments are `.putslot!` are the object,
   the slot name, and a spec, which can be one of:
   - `($constant-slot-spec value)` for a constant value;
   - `($thunk-slot-spec thunk)` for a zero-argument function that
     computes the value;
   - `($self-slot-spec fun)` for a one-argument function that
     computes the value from the instance `self`;
   - `($computed-slot-spec fun)` for a two-argument function that
     computes the value from the instance `self` and
     a function `superfun` that computes the inherited slot value.
   ```
   (.def o a: 1)
   (.def (p @ o) b: 2)
   (.putslot! o 'c ($constant-slot-spec 3))
   (.putslot! o 'd ($thunk-slot-spec (lambda () (+ 3 4))))
   (.putslot! o 'e ($self-slot-spec (lambda (self)
       (map (lambda (slot) (.ref self slot)) '(a b c d)))))
   (.putslot! o 'a ($self-slot-spec (lambda (self superfun) (+ 1 (superfun)))))
   (assert-equal! (.@ p e) '(2 2 3 7))
   ```

4. The `.def!` special form

5. The `.putdefault!`


1. The `.def!` form adds a slot definition after the fact to an existing object prototype,
without changing the instance; it will only affect instances using the prototype
if they haven't used the previous definition yet.
```
(.def foo (x 1))
(.def! foo y (x) (+ x 3))
(assert-equal! (.get foo y) 4)
```

The `.set!` form modifies the value of an object instance without changing the prototype.
```
(.def bar (x 1))
(assert-equal! (.get bar x) 1)
(.set! bar x 18)
(assert-equal! (.get bar x) 18)
```

For reflection on what slots an object does or doesn't define, two functions are available:
```
(assert-equal! (.key? foo 'y) #t)
(def (sort-symbols symbols) (sort symbols (λ (a b) (string< (symbol->string a) (symbol->string b)))))
(assert-equal! (sort-symbols (.all-slots foo)) '(x y))
```

A short-hand for one or multiple nested checks of `.key?` with unquoted key is available:
```
(.def baz (a (.o (b (.o (c 1))))))
(assert-equal! (.has? baz a b c) #t)
(assert-equal! (.has? baz a d) #f)
```

## Examples

### Simple definitions and usage

The following form defines a point with two coordinates `x` and `y`;
the first three empty lists stand for the omitted `self` variable,
the empty list of super-prototypes, and the empty list of extra slots:
```
(def my-point (.o (x 3) (y 4)))
```
Similarly, here is a prototype object for a colored object,
using the short-hand `.def` special form:
```
(.def blued (color 'blue))
```
The two can be combined in a single object using the function `.mix`:
```
(def my-colored-point (.mix blued my-point))
```
You can verify that the slots have the expected values:
```
(assert-equal! (.ref my-colored-point 'x) 3)
(assert-equal! (.ref my-colored-point 'y) 4)
(assert-equal! (.ref my-colored-point 'color) 'blue)
```
You could use `.get` instead of `.ref` to skip a quote:
```
(assert-equal! (.get my-colored-point x) (.ref my-colored-point 'x))
```

### Simple mixins

This mixin defines (using `.o` syntax) a complex number `x+iy`
for an object that with slots `x` and `y`,
to be defined in a different mixin (note the use of `@` as an unused symbol for self):
```
(def complex (.o (:: @ [] x y) (x+iy (+ x (* 0+1i y)))))
```
And this mixin defines (using `.def` syntax) polar coordinates for an object with a slot `x+iy`:
```
(.def (polar @ [] x+iy) (rho (magnitude x+iy)) (theta (angle x+iy)))
```
You can mix these together and see POO at work:
```
(assert-equal! (.get (.mix colored-point polar complex) rho) 5)
```

### More advanced slot definitions

A slot defined from the lexical scope:
```
(let ((x 1) (y 2))
  (.def point (x) (y))
  (assert-equal! (map (cut .ref point <>) '(x y)) [1 2]))
```

A slot defined by modifying the inherited value:
```
(.def gerbil-config
  (modules => prepend '(gerbil gambit)))
(def (prepend x y) (append y x))
(.def base-config
  (modules '(kernel stdlib init)))
(assert-equal! (.get (.mix gerbil-config base-config) modules)
               '(gerbil gambit kernel stdlib init))
```

A slot defined by conditionally using the inherited computation:
```
(.def (hello @ [] name)
  (language 'en)
  (greeting (format greeting-fmt name))
  (greeting-fmt "hello, ~a"))
(defpoo (localize-hello @ [hello] language)
  (name "poo")
  (greeting-fmt (next) (if (eq? language 'fr) "salut, ~a" (next))))
(defpoo (french-hello @ localize-hello)
  (language 'fr))
(assert-equal! (.get localize-hello greeting) "hello, poo")
(assert-equal! (.get french-hello greeting) "salut, poo")
```

### Further examples

There are more examples are in the file [`object-test.ss`](../t/object-test.ss).

## Future Features

There are infinitely many possible improvements. See [`TODO.md`](TODO.md) for a few salient ones.

## Implementation Notes

TODO: document default slot values, mutiple inheritance, etc.
