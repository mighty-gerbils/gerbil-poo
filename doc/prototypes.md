# Prototype Object Programming in Gerbil Scheme

By François-René Rideau. 2020-11-07.
https://www.meetup.com/LispNYC/events/vqhmbpybcpbnb/
https://youtu.be/ckXkIlhUWss

What is the essence of object-oriented programming?

Incremental composition of code and data fragments with late binding.

We will summarize the theoretical idea in 99 characters of Scheme code.
We will explain how the very same design was achieved before
by [Jsonnet](https://jsonnet.org/) and
[Nix](https://github.com/NixOS/nixpkgs/blob/master/lib/fixed-points.nix):
object instances as trivial fixed-points of prototypes,
themselves simple composable mixins in a dynamically-typed pure lazy functional language.

We will also present [POO](https://github.com/fare/gerbil-poo),
a practical implementation of Prototype Object-Orientation
in a few hundreds of lines of [Gerbil Scheme](https://cons.io).
We will demonstrate how this design can express Class-based Object-Orientation or Typeclasses
as simple prototypes for type descriptors (hence the need for dynamic types or reflection),
and how it enables both parametric polymorphism and ad hoc polymorphism
in [Interface-Passing Style](http://github.com/fare/lil-ilc2012/).
We will show how in practice we use this design
to factor our code into extremely succinct components that we can reuse compositionally.

We will briefly mention not-yet-implement projects
to overcome limitations to this implementation, including
the handling of inheritance hierarchies as DAGs rather than manually linearized lists,
method composition, typing, as well as many shaping and caching optimizations.


```scheme
(define (new p b) (letrec ((f (p (λ a (apply f a)) b))) f))
(define ((inhr p q) f s) (p f (q f s)))
```


## Intro: A bit of History

When they think about "object-oriented programming", most people automatically think of "classes".
Objects with classes have a long tradition:
SIMULA (1967), Smalltalk (1972),
[Flavors](https://en.wikipedia.org/wiki/Flavors_(programming_language)) (1982),
C++ (1985),
Python (1991),
Perl 5 (1994),
Java (1995),
Scala (2004).

But you can do objects without classes.
It's much simpler, and much more powerful:
you can implement classes very easily on top of prototypes,
but you cannot implement prototypes on top of classes.
Instead, you can implement prototypes very simply with fixed-points of higher-order functions
--- assuming you have dynamic types and lazy evaluation.

The most famous and most popular language with prototype objects is... JavaScript (1995)!
Its not-so-well-designed object system gave prototypes a bad rap
and many people prefer to use some class system on top;
yet a lot of people rely on some prototype-specific features of the object system,
if only under the hood of their favorite framework.

Now the history of [prototypes](https://en.wikipedia.org/wiki/Prototype-based_programming)
goes at least back to 1979 in the tradition of Smalltalk and its descendants:
[ThingLab](https://github.com/cdglabs/thinglab) (1979),
[Self](https://selflanguage.org/) (1987) that with funding from Sun put prototypes on a lot of radars,
[Slate](https://github.com/briantrice/slate-language) (2008).

There have mean many
[prototype](https://web.media.mit.edu/~lieber/Lieberary/OOP/Delegation/Delegation.html)
[object](https://en.wikibooks.org/wiki/Scheme_Programming/Object_Orientation)
[systems](http://community.schemewiki.org/?object-systems)
in Scheme over the years:
[T](https://en.wikipedia.org/wiki/T_(programming_language)) (1981),
[YASOS](http://people.csail.mit.edu/jaffer/SLIB.html) (1992),
[Protobj](https://www.neilvandyke.org/racket/protobj/) (2005),
[Prometheus](https://github.com/jorgenschaefer/prometheus) (2005),
[TinyTalk](https://launchpad.net/kend) (2008).

Also many prototype object systems in other Lisps:
Object Lisp (1985),
[ABCL](https://en.wikipedia.org/wiki/Actor-Based_Concurrent_Language) (1986),
Sheeple (2008).

And even many other, "blub", languages with prototypes, notably:
BETA (1983) (you can squint and see its patterns as prototypes),
Cecil (1992),
Obliq (1993),
NewtonScript (1993),
Lua (1993),
E (1997),
REBOL (1997),
OpenLaszlo (2001),
Io (2002),
Red (2011).

There have been many academic articles about prototype object systems in Computer Science literature:
many articles in the 1980s, some in the 1990s, a few since.
They have their chapter in the book by Abadi & Cardelli (1996).
A good article is Norman Adams and Jonathan Rees'
[Object-Oriented Programming in Scheme](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.46.954)
(1989): it describes a portable Scheme reimplementation of T's prototype object system,
which is essentially isomorphic to the system I'll be presenting.
This is the basis for YASOS and TinyTalk (I didn't check Prometheus or Protobj, but they seem similar).

But I'm here to tell you about the *pure* thing.
The very essence of prototype object systems, and thus of object-oriented programming itself,
as reduced to its simplest and purest form, in the fewest lines of code.
The first modern variant of it stripped of most non-sense is probably
[Jsonnet](https://jsonnet.org/) (2014), in which the object system was builtin.
And the purest form of it, maybe the most widely used under the hood, is in
[Nix](https://nixos.org/nix) [fixed-points](https://github.com/NixOS/nixpkgs/blob/master/lib/fixed-points.nix) (2015), in which the entire object system is implemented in a dozen lines of code.

## Prototypes: the Jsonnet model

Here is the first example of using prototypes in the language [Jsonnet](https://jsonnet.org/),
which has a syntax inspired by JSON and Python, but
pure functional lazy semantics.

```jsonnet
{
  person1: {
    name: "Alice",
    welcome: "Hello " + self.name + "!",
  },
  person2: self.person1 { name: "Bob" },
}
```

it creates the following JSON record:

```json
{
  person1: {
    name: "Alice",
    welcome: "Hello Alice!"
  },
  person2: {
    name: "Bob",
    welcome: "Hello Bob!"
  }
}
```

More generally:

  * An object can be seen in *two* ways,
    1. as a queryable instance, where fields have values, and
    2. as a composable prototype, where fields have open computations.

  * The open computation of a field can refer to other fields in "this" object,
    or to the same field (or other fields?) in the "super" object
    with some special accessor like `call-next-method`, `this.foo`, `super.bar`, etc.

  * The instance is deduced from the prototype by taking the fixed points of its computations,
    by closing the loops in the computations so that field references are resolved
    into the suitably named computations.

  * Composition ignores the instance and combines computations by "inheritance" (see below),
    overriding some of the field computations with further computations that
    may refer to the previous "super" computation.

In a typed language, the two entities record and prototype have sharply different types,
and you may want to clearly distinguish them.

<!–- comment –->
[comment]: # λ←→∈⊂⊆⊊

```
fix = λ f → let x = f x; in x;

prototype [A⊂B] = A ← A B

instantiate[A⊂B] : A <- prototype[A⊂B] <- B
instantiate p b = fix (λ a → p a b)
instantiate p b = self where self = p self b
instantiate p b = (λ f a → f f a) (λ a → p a b) b
instantiate p b = letrec f = p f b in f
instantiate p b = letrec f = p (λ a → f a) b in f

inherit[A⊂B⊂C] : prototype[A⊂C] <- prototype[A⊂B] prototype[B⊂C]
inherit p q a b = p a (q a b)
```

However, in a *pure* *lazy* language,
all instances of a prototype there is a unique instance up to deep equality.
Therefore, we can safely "identify" the object and the prototype and/or implicitly bundle them together.
Extracting a field from an object will implicitly (and lazily) compute the instance from the prototype.
Lazy also means that it's OK for some objects to have a prototype without an instance
that computes a valid value for every field finite time or at all, e.g.
```
let cartesian_to_complex_mixin = { z : x + i*y }
let cartesian_to_polar_mixin = cartesian_to_complex_mixin { ρ = mag(z) ; θ = atan(x, y) }
let increase_super = { a : super.a + 1 }
```

This comes super handy in Jsonnet or POO when you define nested and parametrized mixins and
don't have to decide explicitly and eagerly *when* to instantiate:

```
let config x = {
  TODO
}
```



## Prototypes: the trivial Nix reimplementation

TODO: Get examples from Nix

Interestingly, in Nix, unlike in Jsonnet, prototypes are not a builtin language concept,
but something implemented in the standard library in fewer than 10 lines of actual code:

```nix
TODO: extract the code from lib/fixed-points.nix
```

## Prototypes: Two-liner in Scheme

We're Lispers, so let's try to do the same thing in Scheme.
If we are to reduce the thing to its essence, what gives?

TODO: the 99 characters.

Decompressed into a few more lines, with comments.
TODO: decompressed code

Actually, in Scheme, the part that is hard is not Prototype inheritance,
it's reimplementing (pure, lazy) records on top,
since they are not a language primitive. And that admittedly

## Prototypes in Scheme, model T

Object as function from symbol plus self plus super plus method arguments to method result value.

## POO

Object as bundle of an inheritance list of prototypes and an instance
instance as hash-table cache of already-computed values
prototype as hash-table of methods
methods as function from self plus super method to value


## Classes on top of prototypes

(Type)Class Dictionary

Type Descriptors


## Better design patterns with Mixins

https://en.wikipedia.org/wiki/Software_design_pattern

Nicer than in Racket, in JS,

decorator pattern, delegation pattern, etc.
in wrappers in Java.

See Marshalling, Sum types, etc.


## Tries and Persistent Tries



## Static Types?

* Monomorphic prototypes
* Type system https://github.com/fare/projects/issues/3


## Beyond mere prototypes

* MOP https://github.com/fare/projects/issues/7
  * Flavors- or CLOS-style method combination
  * For inheritance list, a linearized inheritance DAG.

* Generating multiple aspects

* Hygienic identifiers, not symbols, as field labels?
  Enables aspect-based metaprogramming
  to combine multiple aspects into a common spec

* Combine with partial evaluator to inline away most structure at compile-time?
