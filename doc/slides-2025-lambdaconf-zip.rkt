#lang at-exp racket @; -*- Scheme -*-
#|
MerkleProofing, where FP meets Algebra, Calculus Metaprogramming and OO, or, How Math can Automate Your Programs

Slides for presentation at LambdaConf 2025 in Estes Park, Colorado, 2025-05-12

To compile it, use:
  mkdir -p ../run
  ln -s ../../nada-poof/util ./
  ln -s ../../nada-poof/resources ../run/
  racket slides-2025-lambdaconf-zip.rkt > ../run/slides-2025-lambdaconf-zip.html
where nada-poof is a checkout of
  https://github.com/metareflection/poof

To test interactively, try:
  racket -i -l scribble/reader -e "(use-at-readtable)" -l racket

This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html
|#

(require scribble/html
         "util/util.rkt"
         "util/coop.rkt"
         (rename-in "util/coop.rkt" (|@| $))
         "util/protodoc.rkt"
         "util/reveal.rkt")

(def (haddock x)
  (img src: (format "pics/haddock~a.webp" x) alt: (format "panel ~a" x)
       height: "250vmin" valign: 'top))

(def doc
  (docfix
    ($title "MerkleProofing")
    ($kv 'slide
      (list
       @div[class: 'logo]{
       @img[src: "resources/pic/mukn-name.svg"
            alt: "Mutual Knowledge Systems"
            width: "50%"
            valign: 'middle
            style: "
    vertical-align: middle;
    background-color: white;
    padding-left: .5em;
    padding-right: .5em;
    padding-top: .5em;
    padding-bottom: .5em;
"]}
       @; TODO: hide the menu behind a button at the top when on mobile (?)
       @div[class: 'title
            style: "color: #55f; vertical-align: middle; text-align: center; font-size: 140%"
            @b{@br MerkleProofing}]
       @(br clear: 'all)
       @p{where FP meets Algebra, Calculus, @br Metaprogramming and OO}
       @p{or}
       @p{How Math can Automate Your Programs}
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>")}
       @C{@small{@Url{http://github.com/mighty-gerbils/gerbil-persist}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{LambdaConf 2025-05-12}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))
    ($section "Prelude: The Path, Not The Destination"
     $plan-slide
     ($slide "Thesis"
        @L{Use zippers for speed, simplicity or merkleization}
        @L{Every data structure could/should come with autozippers}
        @L{Today advanced maths, tomorrow table stakes}
        @L{Synergy: FP, Algebra, Calculus, Metaprogramming, OO…})
     ($slide "Merkleization"
        @L{Assume 1-way hash function}
        @L{Use hashes as pointers into immutable nodes of a DAG}
        @L{git. Content-Addressed Storage. Deduplication. @br P2P networks. Timestamping. Blockchains.}
        @L{Merkle Proofs: Certifying partial data structures})
     ($slide "Merkle Proofs Illustrated"
        @L{How to exhibit a substructure from a committed hash?}
        @C{< Insert Diagram >}) ;; Binary Tree example. from GNUtella
     ($slide "Zippers to the Rescue!"
        @L{How to represent focusing on a subtree?}
        @C{< Insert Diagram >})
     ($slide "From Cool Hack to Systematic Technique"
        @L{Huet 1997: Cool Hack}
        @L{McBride 2001: Systematizing it}
        @L{Joyal 1981, Yorgey 2010: Deeper Theory}
        @L{Tomorrow: Basic Feature in all PLs}))
    ($section "Algebra, Calculus... for Data Structures"
     $plan-slide
     ($slide "Finite Data Types" ;; Combinatorial Species
        @L{Generated from: 0 +1}
        @L{Semi-ring: 0 1 + ×}
        @L{… Isomorphic to ℕ}
        @L{Exercise: ℤ, ℚ, ℝ, ℂ}) ;; -2/7 means "but not on weekends"
     ($slide "Beyond Finite Data Types"
        @L{Type variables: a b c… ⇒ Polynomials}
        @L{Variables: quantify over any category of objects}
        @L{Infinite types: unlike numbers} ;; what are morphism? not going to elaborate on that
        @L{Exponential: b ⟶ a = a ⟵ b = aᵇ}) ;; total function types
     ($slide "Simplest Infinite Data Types"
        @L{(Pure) Inductive Data Types}
        @L{Least Fixed Point µ(P)}
        @L{P(µ(P)) = µ(P)}
        @L{µ(P) = min{f | P(f) = f}}) @; | }})
     ($slide "Example: Lists"
        @L{List(a) = 1 + a×List(a)}
        @L{P(a) = x ↦ 1 + a×x}
        @L{List(a) = µ(P(a))}
        @C{< Insert Diagram >})
     ($slide "Example: Binary Trees"
        @L{BinaryTree(a,b) = a + b×BinaryTree(a,b)×BinaryTree(a,b)}
        @L{Q(a,b) = x ↦ a+b×x×x}
        @L{BinaryTree(a,b) = µ(Q(a,b))} ;; Infinite types!
        @C{< Insert Diagram >})
     ($slide "Algebra: Thinking in Structures"
        @L{Two levels: sets, elements}
        @L{Elements are interchangeable, sets are not} ;; "up to isomorphism". In a ring, not all elements interchangeable
        @L{Structures are where the fun is}
        @C{… Category Theory}))
    ($section "Differential Calculus... for Data Structures"
     $plan-slide
     ($slide "Differentiating a Data Type"
        @L{P(X) = sum of product, choice between records}
        @L{∂Y/∂X = type of holes of type X in Y}
        @L{Zipper Context, (Zipper) Path}
        @C{< Insert Diagram >})
     ($slide "Usual Differentiation for Polynomials!"
        @L{P(X) = ∑ aₙXⁿ}
        @L{P'(X) = ∑ n aₙ Xⁿ⁻¹}
        @C{< Insert Diagram >})
     ($slide "What of Inductive Data Types?"
        @C{< Insert Diagram >})
     ($slide "Inductive Types Constructively"
        @L{µ(P) = min{f | P(f) = f}} @; | }}
        @L{M(P)(T) = lim Pⁿ(T)}
        @L{µ(P) = M(P)(0)}
        @L{M(P)'(T) = ∂M(P)(X)/∂X (T) = List(P'(T))}
        @C{< Insert Diagrams >})
     ($slide "Zippers"
        @L{T=µ(P)=M(P)(0)}
        @L{zipPath(T) = M(P)'(T) = List(P'(T))}
        @L{zipper(T) = T × zipPath(T) = T × List(P'(T))}
        @C{< Insert Diagrams >})
     ($slide "Reified Optics"
        @L{zipPath = negative-focus lens as data}
        @C{< Insert Diagram >}))
    ($section "Benefits of Zippers"
     $plan-slide
     ($slide "Faster"
        @L{O(1) focus access}
        @L{O(1) amortized sequential access}
        @L{O(1) “local” access}
        @L{@em{Dynamically} discover locality})
     ($slide "Simpler"
        @L{10x less code than Ethereum's Tries in Go}
        @L{2x features: incremental access, sequential access, @br
                        binary methods, merkleization…}
        @L{Don't write the code twice}
        @L{Adopt Zippers vs nodes as primary data structure?})
     ($slide "Merkleization in 2 lines of code"
        @L{T=µ(P)}
        @L{ZipPath=List(P'(T))}
        @L{Proof=List(P'(H))}
        @L{@code{getMerkleProof k t = zipperOf t >>= refocus k @brii
                    >>= zipPath -. fmap digest -. return
                 }})
     ($slide "Composable, Reified"
        @L{Part of Optics}
        @L{Reified as Data, not just Opaque Functions}
        @L{@em{Dynamic} simplification vs memory leak}
        @L{@em{Dynamic} permission checks}))
    ($section "Metaprogramming, OO"
     $plan-slide
     ($slide "Zippers by hand are error-prone"
        @L{But: Differentiation is a global property of data type}
        @L{Can be plugged by hand at the end in a parameter}
        @L{OO: incrementally refine your data type}
        @C{“I object to doing things that computers can do” @br — Olin Shivers})
     ($slide "Automatic Differentiation of Types"
        @L{Type-directed Metaprogramming}
        @L{Scheme: function on first-class type descriptors}
        @L{Haskell: possible, not easy}
        @L{Ad hoc vs general purpose metaprogramming}))
    ($section "Conclusion: The Essence of LambdaConf"
     $plan-slide
     ($slide "Thesis (Recap)"
        @L{Use zippers for speed, simplicity or merkleization}
        @L{Every data structure could/should come with autozippers}
        @L{Today advanced maths, tomorrow table stakes}
        @L{Synergy: FP, Algebra, Calculus, Metaprogramming, OO…})
     ($slide "My Most LambdaConf Talk"
        @L{2016: Orthogonal Persistence & Reflection}
        @L{2017: Better Stories. Software Evolutionism}
        @L{2018: DApps & Game Semantics. 1ˢᵗ-Class Implementations}
        @L{2019: Blockchain DApps in FP}
        @L{2024: Prototype OO Functionally}
        @L{2025: Orthogonal Persistence. Math for Programming})
     ($slide "The Essence of LambdaConf"
        @C{“The hacker: someone who figured things out @br
                and made something cool happen.” @br
                — Alan Schmitt})
     ($slide @list{Math is Practical}
        @L{The Heart of @a[href: "https://SkyProtocol.org"]{SkyProtocol.org}}
        @L{@a[href: "https://github.com/mighty-gerbils/gerbil-poo/blob/master/doc/triezip.md"
                    ]{github.com/mighty-gerbils/gerbil-poo doc/triezip.md}}
        @L{X: @Url{https://x.com/ngnghm}}
        @L{Blog: @Url{https://ngnghm.github.io}}
        @C{@code{<fare@"@"mukn.com>}}))))

(reveal-doc doc)


#|
TODO:
more examples
more graphs


Need to explain to people not already familiar with area


Use inkscape or

     ($slide "XXX"
        @L{XXX}
        @L{XXX}
        @L{XXX}
        @L{XXX})
|#
