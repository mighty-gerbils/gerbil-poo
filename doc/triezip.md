# Don't Zip it All Up!

Or, why and how to partially zip up and down a trie for fun and profit.
We use Huet's familiar
[zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)) idea
to incrementally manipulate [tries](https://en.wikipedia.org/wiki/Trie)
in `O(1)` amortized time per batch operation (`O(N)` total),
instead of `O(log N)` time per batch operation (`O(N log N)` total)
as would be the case with a naive use of the regular elementary accessors.
*In practice, this leads to a 10x speed-up for batch operations
on tries with millions of elements.*

In the presentation below and the accompanying code,
I am using [Gerbil Scheme](https://cons.io) with my own
prototype object system [POO](https://github.com/fare/gerbil-poo)
to represent runtime descriptors for dynamically-enforced dependent typeclasses.
But the same design can be readily adapted (with more or less pain, depending)
to whichever language your blockchain (or non-blockchain) codebase is using.
Indeed, we at *[Mutual Knowledge Systems](https://mukn.io)
can help you and your development team adopt this technology*
through our consulting, development and training services.

Finally, note that we only claim originality in the particular way we combine
the design elements below to create solutions to specific practical problems.
The general ideas of tries, of pure data structures, of zippers,
of incremental computations, types, parametric types,
covariance and contravariance, dependent types, type descriptors, etc.,
are each well established, decades old technologies,
though not often seen all together.
Still, the presentation below may sometimes use slightly different terminology
from others who have published articles or codes about zippers;
sometimes because there is no widely accepted terminology, other times because
what terms may be widely used can be ambiguous or confusing,
such as the word “context” itself which in context
can be either very clear or very vague
(and so for instance we call `Path` was Huet calls `Context`).

## Tries

### Tries 101

Big-Endian Patricia Trees, or Tries,
represent a finite table mapping integer keys to arbitrary values,
as a full binary tree of a given height,
where some nodes are marked as omitted (or `Empty`),
signifying no mapping for the omitted keys.
A good article about them is
[Fast Mergable Integer Maps](http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps)
by Chris Okasaki & Andrew Gill, 1998.

Tries have many advantages:

  1. Like many other balanced tree data structures, they guarantee that
     all elementary access functions (such as reading or modifying a mapping)
     will have only logarithmic worst case complexity
     in the size of the keys and/or the size of the data.

  2. Unlike most other balanced trees, tries provide
     a *canonical* representation for any mapping:
     two equal mappings will always be represented by structurally equal tries.
     By contrast, red-black trees, AVL trees, B-trees, etc.,
     may be faster for many use cases,
     but which tree you obtain will depend on
     the history of insertions and modifications.

  3. This canonical representation in turn makes it possible to easily
     compare mapping tables, deterministically agree on their structure
     despite concurrent evaluation paths, compute a well-defined digest
     for a mapping, use hash-consing to ensure unique copies of tables, etc.

The latter property of tries makes them particularly well-suited for use
in blockchains and other distributed ledgers, where participants can agree on
a bitwise-identical representation of the state of some common data structure,
independently from the specific order of operations that each of them used
to reach that state.

### Keys and Height

The type `Value` of values is an arbitrary parameter
of the `Trie` data structure; however,
the type `Key` of keys must be a subtype of the type `Nat` of integers.
Typically, for content-addressed or intent-addressed storage
using a hash function producing a digest of size `N` bits,
type `Key` will be `UInt(N)`, e.g. `N==256` when using SHA-256 (like Bitcoin)
or Keccak-256 (like Ethereum).

Actually, tries can be conceptually extended to accept signed integers as keys,
by considering that the type `Z` of (signed) integers
is isomorphic to `Nat + Nat`,
with a second copy of `Nat` for negative integers in two's complement.
Thus, a trie with `Key==Int` is actually a pair of tries with `Key==Nat`.
In the discussion below, we'll use the simplifying assumption that `Key <: Nat`.

Now, for every integer `n`, we can define the height of `n` as
`(1- (integer-length n))`, and similarly, for any vector of bits,
its height will be one less than the length of that vector.
That's the amount by which to logically shift `1`
to reach the top bit in that integer (or, for `n==0`, it's `-1`),
it's the index of the last bit in the bit vector.
We can then define the type `Height` as the type containing
the heights of the positive integers in `Key`.
For `Key==Nat` we have `Height==Nat`.
For `Key==UInt(2**N)` we have `Height=UInt(N)`;
in particular for `Key==UInt256`, we have `Height==UInt8`.
Logarithms can be generalized to types or parameterized types
as the inverse of exponentials or of exponential functors,
and we can thus write:

    (deftype Height (Log2 Key))

Making height one less than the `integer-length` leads to
minor space and time saving compared to directly using that integer-length:
for instance, we can store all heights for a `Key==UInt256`
with numbers from 0 to 255 or type `UInt8`,
instead of numbers from 1 to 256 as would happen with using `integer-length`,
which could require an extra byte.

### Trie type definition

The simplified data type representation I'll be using for tries is:

```scheme
(define-type Trie
  (Sum (Empty)
       (Leaf Value)
       (Branch Height Trie Trie)))
```

In this variant, each internal trie node carries its own height,
which avoids having to have a separate context object
and/or special top trie node to carry the height information,
thus decreasing the number of entities required in the following discussion.
But you can save one byte per node (plus padding)
by not redundantly carrying height information everywhere.

A further space optimization we use in our actual implementation is
to define another alternative kind of node,
`(Skip Height Height Key Trie)`, that will “skip” over a bunch of bit keys,
replacing any series of consecutive `Branch`es where one arm is `Empty`.
Then, `Empty` is forbidden anywhere but at the top, and
the number of nodes in the tree remains proportional to the number of leaves,
when, without this optimization, it would increase with the height of the tree,
which grows logarithmically with the greatest index in the tree.
This optimization is largely orthogonal to the rest of this discussion,
so I will simplify it away in most of this write-up.

In any case, tries are not a “free” representation like arbitrary binary trees,
but are heavily constrained with a somewhat elaborate invariant.
The invariant involves data such as height that may be redundantly included in
each individual node
(as in the representation used in this paper and this repository),
or might instead be included only in
some global contextual information “head” node.
For those familiar with the correspondance
between number representation systems and data containers,
tries correspond to the familiar binary representation system.

## Trie Zippers

### Zippers 101

In his [classic 1997 article](http://www.st.cs.uni-sb.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf),
Gérard Huet proposes to “unzip” or “zip down”
some arbitrary recursive tree-like data structure
down a spine (a series of nodes from the top),
to focus on a “current” internal node.
As you change your focus, you remember the path you took to get there,
with all the information you need to “zip up” the zipper
and retrieve the original data structure,
corresponding to all the branches *not* taken.

You may even zip down to a node,
do some operation that modifies an internal node, then zip it up
to obtain a new data structure based on the previous one,
but where the internal node was edited.
Thus, a zipper is quite similar to the recently popular lens data structure,
except that a zipper is reified as data that you can construct and deconstruct,
when a lens is abstracted as functions
that you can trivially compose but not decompose.
Indeed, you can trivially extract a lens from a zipper,
but not the other way around.

If you squint, you can view the usual accessors on a data structure as
implicitly using a zipper “all the way down” to reach some individual leaf data,
then, after modification, back “all the way up”
to return an updated data structure.
Zippers may even be used explicitly,
in the internals of the implementation of those accessors.

But the real power of zippers is in keeping them partly zipped or unzipped,
and edit a lot of nodes in a way that preserves locality
and thus moves the zipper as little as needed.
Thus, instead of each access costing `O(log N)` on average,
the *amortized* cost of accessors would be `O(1)`.

### Parameterized Zippers

In the representation of zippers I chose, I explicitly separate
the information for a node from the information of
the `Path` that was taken down and may be taken back up in reverse.
Note that some Haskell libraries, after Huet's article,
calls this path a “context”,
but that's too general a word to use out of context (sic)
to mean something so narrow as a path.

```scheme
(define-type (Zipper a) (Pair a (Path a)))
```

Also note that I make my path parametric in the type `a`
of information I care about regarding the tries.
To simply navigate, read and modify a trie,
said type parameter would be the type `Trie` itself.
To count the number of leaves, it could be a `Nat`
(or `Key+1` so you can count full tries).
To both count the number of leaves and read and modify the table,
if would be `(Pair Nat Trie)`.
To merkleize a trie, the type parameter would be the type `Digest`
as returned by the cryptographic hash function.
And so on.

Note that the relationship between `a` and the intended `Trie` is not arbitrary.
When counting the leaves in a `Trie`, or computing any
[*synthesized* attributes](https://en.wikipedia.org/wiki/Attribute_grammar)
of the tree, i.e. information that propagates *up* from the leaf to the root,
you need a function of type `a <- Trie`
to compute your information from the subtrie so far.
(NB: note how the previous sentence involved
two mutually contravariant metaphors for “up and down” vs “leaf and root”.)
On the other hand, to be able to navigate down (left and right) as well as up,
which is needed when computing any
[*inherited* attributes](https://en.wikipedia.org/wiki/Attribute_grammar)
of the tree, attribute of the tree, you need a function of type `Trie <- a`
to reconstitute the subtrie from the information so far.
And when computing both synthesized and inherited attributes at the same time,
you essentially need `a` to be `Trie`, or isomorphic to `Trie`.

If you are familiar with functional optics,
parameterized Zippers are to unparameterized zippers
what `s t a b` Van Laarhoven lenses are to simple `s a` lenses:
they allow you to reason about not just about
incrementally accessing data from a trie, but also about
incrementally computing a function that maps a trie into something else.
However, to usefully compose arbitrary zippers across several kinds of
nested data structures, such as a list of tries of some data structure
containing integer in some fields,
you probably need existential types as
you cannot in general predict what the next step will be:
was the previous step in path descending another trie node,
was it extracting the element from a list of trie nodes,
or was it something else involving
an entirely different data structure containing tries?
Lenses don't need to worry about that
because they are not decomposable, only composable, so they can
hide intermediate steps and path transitions behind type abstractions.
But Zippers may in general need at least existential types,
possibly in some non-modular whole-program GADT, and
in the most general case might require dependent types.

### Representing Trie Zippers

Huet's article explains how, for data structures freely generated
from a set of generators specified by an algebraic equation,
the path data structure (that he calls context) can be deduced
from the equation via some abstract differentiation.
However, as described above, my representation for tries
is quite constrained by subtle invariants and not “freely generated”,
with tradeoffs between several technical concrete choices
for representing keys, heights, skips, etc.
A representation for the path will similarly involve similar choices,
and here are those I made.

First, I define a type `Costep` that contains contextual information
about the height of the current focus on a sub-trie, and
the high bits of the key leading from the top to the current node in focus:

```scheme
(define-type Costep (Pair Height Key))
```

Note how a related type might be used as the “head” of a trie,
in the case that you don't store the height in every node.

My second type, `Step`, contains for one branch taken from the top
some information about the branch *not* taken.
In conjunction with a bit from the `Costep` above,
it can be used to track and/or undo the step that was taken.
It can be defined as just:

```scheme
(define-type (Step a) (Sum (BranchStep a)))
```

The type for the full context is then:

```scheme
(define-type (Path a) (Pair Costep (List (Step a))))
```

As explained in the section above, the `Step` and `Path` data structures are
parametrized with the type of information associated with branches not taken.

In the version of tries with `Skip` nodes,
another alternative for the `Step` sum can also be a `(SkipStep Height)`,
which does not depend on the parameter `a`, since there are
no other non-empty branches to store information about in a `SkipStep`.
On the other hand, there are no alternatives corresponding to `Leaf` or `Empty`,
since you cannot "descend" from them into a further trie.
Still, there *would* be some step corresponding to `Leaf`...
in a generalized cross-data-structure zipper
that might lead from a `Trie` to a contained `Value`;
yet there still would not be any step corresponding to `Empty`,
since it would still not contain any data to descend into.

We could have distributed the key information in the steps,
by having a `bit` field in each `BranchStep`,
or equivalently a `LeftBranchStep` and a `RightBranchStep`.
(Similarly, a `SkipStep` would have a `bits` field).
That would make some key operations much slower in practice,
since it's much cheaper to do bit field manipulation on integers
than walking over lots of steps to manipulate bits one by one.

We could also have had each `Step` contain a `height` field,
but there again that would be redundant in most cases,
and you'd still need height information in the base case or an empty list,
which would have been awkward, and/or would have required
using our own variant of lists instead of actual lists.

A carefully chosen encoding for the zipper context
can thus affect performance in practice,
compared to an encoding automatically extracted from the original data structure
based on a naïve algorithm.
A non-naïve algorithm to chose suitable runtime encodings
for arbitrary data structures and their zippers
is left as an exercise to the reader or his AI.

### Unzipping one bit

Now, we're almost equipped to zip around our trie data structures.

To reconstitute the next node up from a zipper,
we can simply apply to the sub-`Trie` in focus
the next `(Step Trie)` in the path together with the `Costep` information,
which would return the next `Trie` up and the updated `Costep` information
with its height one higher and its key bits one shorter
(or, if we use `SkipStep`s,
the height higher and the bits shorter by some given length,
being the given height of the step plus one).
The signature of the function that processes one step would be:

```scheme
apply-step : (Fun Trie Costep <- Trie Costep (Step Trie))
```

But what of the more general case when we use
parameterized `(Path a)` and `(Step a)`, instead of
just `(Path Trie)` and `(Step Trie)`?
Then we need some extra “unstep” parameter
that explains how to undo a step down into a step up given those parameters:

```scheme
(define-type (Unstep trunk branch)
  (Struct left: (Fun trunk <- Key Height trunk branch)
          right: (Fun trunk <- Key Height branch trunk)))
```

Note that `Unstep` has two type parameters, that correspond to
the `trunk` information we keep about the trie in focus, and
the `branch` information we keep about the untaken branches up the path.
Then, the `apply-step` function will have this signature:

```scheme
apply-step : (Fun trunk Costep <- (Unstep trunk branch) trunk Costep (Step branch))
```

Once again, if you use `Skip` nodes,
you'll want another case in your `Unstep` structure,
that given the context's key and height information,
the height and bits of the step, and the information about the focused node,
will return the information about the next node up:

```scheme
skip : (Fun trunk <- Key Height Height Key trunk)
```

You could also encode the regular branch handler as a single case
with a `Bit` parameter, instead of two separate left and right cases:

```scheme
branch : (Fun trunk <- Key Height Bit trunk branch)
```

## Zipping down the lane

### Batch operations

Elementary operations on a trie (like many other balanced trees)
have average and worst-case complexity `O(log N)`
where `N` is the size of the trie
(also best-case complexity, in the case of tries).
When you do a large number of random operations `O(N)` on a trie,
you must thus pay a price `O(log N)` in average for each operation,
or `O(N log N)` overall.

But most of the time, your operations are not random:
they are sequential, or otherwise have a large degree of locality.
Thus, if you use zippers to only minimally navigate the structure of your trie,
you can substantially cut on the average cost of operations,
and keep them in `O(N)` overall, or an `O(1)` amortized average.
In the pun of
[Shan](http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WalkZip1/)
[&](http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WalkZip2/)
[Kiselyov](http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WalkZip3/),
you can zip along your data structure rather than walk along it.

What batch operations can we thus support?
Anything that involves many consecutive accesses to nodes
that are close to each other. This includes but is not limited to:

  - Bulk import of entries into a trie.

  - Bulk export of entries from a trie.

  - Bulk modification of entries in a trie.

  - Scanning for data in a trie.

... Basically, most actual uses of a trie
for which performance actually matters.

### Actual Speed-up

A simple benchmark with our code shows a 5x to 10x speedup
for batches of 1000 to 1000000 operations
on an initially singleton trie of depth 27
vs not using zippers on an otherwise identical code base.

Actually, with our optimized representation using `Skip` nodes,
the actual number of nodes matters more than the total depth,
and with an average of `4` amortized node operations per zipper operation
versus `2 log2 N` amortized node operations without zipper,
with `log2 1000≅10` and `log2 1000000≅20`,
we could have predicted the speedup as a factor `½ log2 N`, or 5x to 10x indeed.
On an actual depth 27 trie, we thus predict the speedup would be more like 13x.

By contrast, in
[the representation used by Ethereum](https://medium.com/shyft-network-media/understanding-trie-databases-in-ethereum-9f03d2c3325d),
that uses 16-fold branch nodes,
a trie with `2**27` entries (one per block so far)
would fit in a trie of depth 7.
At each level we could also batch up to 16 consecutive accesses
in a single node hashing computation,
and would need only `1+1/15` amortized node operations per zipper operation,
versus `2 log4 N` amortized node operations without zipper,
so we can also predict a speedup of about 13x in this case.

### Conclusion

The technique above can be extremely useful to blockchains,
in which tries are an essential data structure;
but really it is useful in any setting where batch operations on trees are used,
whether balanced or not, pure or not, persistent or not,
merkleized or not, Btrees, Patricia trees, Splice trees, etc.

We at [Mutual Knowledge Systems](https://mukn.com)
will gladly consult with your company,
whether in the blockchain space or not,
to implement this technique correctly,
train your team to use it, or more generally help you
adopt better data structures, faster algorithms,
more robust software architecture and safer software development practices
in your business.
