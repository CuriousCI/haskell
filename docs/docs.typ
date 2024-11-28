#set par(leading: 0.55em, first-line-indent: 1.8em, justify: true)
#set page(margin: 1.75in)
#set text(font: "New Computer Modern", lang: "it")
#show outline.entry.where(level: 1): strong
#show outline.entry.where(level: 1): it => {
  show repeat: none 
  it 
}

#align(center + horizon)[
  #heading(outlined: false, text(size: 1.5em, "Haskell"))
  un'implementazione in _StandardML_ \ 

  Carboni Francesco \
  Cicio Ionuţ \
  ?? Giovanni \
  Mazzella Marco
]

#set heading(numbering: "1.1")
#show heading: set block(above: 1.4em, below: 1em)
#show sym.emptyset : sym.diameter 

#pagebreak()

#outline(indent: auto, fill: box(inset: (x: 0.5em), repeat(gap: 4pt)[.]))

#pagebreak()

#let split(over, under, lateral: [], top: 6pt) = box(
  if lateral == [] [
    #grid(
      columns: 1,
      gutter: 6pt,
      over,
      grid.cell(
        stroke: (top: 0.1pt),
        inset: (top: top),
        under
      )
    )
  ] else [
    #grid(
      columns: 2,
      rows: 3,
      gutter: 6pt,
      over,
      grid.cell(
        rowspan: 2,
        align: bottom,
        [ #lateral #v(.6em + top - 1pt) ]
      ),
      grid.cell(
        stroke: (top: 0.1pt),
        inset: (top: top),
        under
      ),
    )
  ]
)

= Haskell

== Grammatica 

Il primo obiettivo è quello di introdurre gli strumenti necessari per poter implementare 3 tipi di programmi: 
- un programma che permetta di verificare che la semantica operazionale sia *lazy* (e non eager)
- un programma che permetta di verificare che la semantica operazionale sia *statica* (e non dinamica)
- un programma che calcoli il fattoriale (per non fare qualcosa di troppo banale)

La grammatica *non è minimale* per rendere più agevole la scrittura in sintassi astratta dei programmi in questa prima fase del progetto.

$ 
  k ::= ZZ | RR | #raw("'a'", lang: "rust") | #raw("'b'", lang: "rust") | ... | #true | #false \
  "Exp" ::= k | x | A ==> B | M = N | M dot N | M - N | \
  "if" B "then" M "else" N | "let" x = M "in" N | "fun" f x = M "in" N | f N \
$



// k ::= 0 | 1 | ... | pi | sqrt(2) | ... | a | b | ... | #true | #false \
// "Haskell" ::= f space.thin x = "Exp" | "module" x ("Haskell"_1, ..., "Haskell"_n) "where" "Haskell"_alpha, ..., "Haskell"_eta | \ "types?" | "classes?" | "instances?"
// "Exp" ::= "function" f x = M "in" N |
// M + N | () tuple literal | 
// "case" Q "of" M_1 -> N_1, ..., M_n -> N_n 

== Semantica operazionale lazy static

L'ambiente per la valutazione assume la forma tipica di una semantica operazionale lazy static: alle variabili sono associati:
- un'espressione da valutare
- l'ambiente in cui valutare l'espressione
La particolarità è che, oltre alle variabili, vengono salvate anche le _chiusure_ delle funzioni:
- il nome della funzione (in questa prima fase non si possono definire funzioni senza nome) 
- il nome dell'argomento
- il corpo della funzione
- il programma da eseguire dopo aver dichiarato la funzione

$ "Env" : "Ident" harpoon.rt^"fin" "Exp" times "Env" union "Ident" times "Exp" times "Env" $

$ arrow.squiggly subset.eq "Env" times "Exp" times "Val" $

// $ arrow.squiggly^p subset.eq "Haskell" times "Val" $
// $ arrow.squiggly^c subset.eq "Haskell" times "Val" $ 

// In base a 'interprete' o 'compilato' cambia la semantica ??

// Considereremo 2 tipi di semantiche per Haskell:
// - quella usata per i programmi (quando si *compila* un file) che richiede la presenza di un ```haskell module``` che esporta la funzione ```haskell main```:
//
// #align(
//   center,
//   ```haskell
//   module Main (main) where
//   main = ...
//   ```
// )
//
// - quella usata dall'interprete per valutare le espressioni 

// === Valutazione di un programma (compilatore)
//
// Per valutare (dare un giudizio operazionale) un programma compilato, è necessario avere il modulo ```haskell Main```. Con questa semantica sto indicando che un funzione di un modulo può vedere tutte le altre funzioni, indipendentemente da dove sono dichiarate 
//
// #align(
//   center, 
//   split(
//     $(f_1, (M_1, emptyset)) space dots.h.c space (f_n, (M_n, emptyset)) tack "main" ()  arrow.squiggly^v v$,
//     $emptyset tack "module Main"("main") "where main", f_1 space.thin x_1 = M_1, ..., f_n space.thin x_n = M_n arrow.squiggly^p v$,
//     // lateral: $("se" "Env"(x) = (M, E'))$
//     top: 8pt
//   )
// )
//
// Sarebbe utile avere una semantica per gli ```haskell import``` e una per portare un modulo in un ambiente, + una semantica per portare una funzione in un ambiente (specialmente per quello interpretato)
// === Valutazione di un programma (interprete)

=== Valutazione di un'espressione 

Di seguito le regole di derivazione

$ E tack k arrow.squiggly k $

#align(
  center, 
  split(
    $E' tack M arrow.squiggly v$,
    $E tack x arrow.squiggly v$,
    lateral: $("se" E(x) = (M, E'))$,
  )
)

#align(
  center, 
  split(
    $E tack A arrow.squiggly #true quad E tack B arrow.squiggly #true$,
    $E tack A ==> B arrow.squiggly #true$,
  )
)


#align(
  center, 
  split(
    $E tack A arrow.squiggly #false quad E tack B arrow.squiggly #true$,
    $E tack A ==> B arrow.squiggly #true$,
  )
)

#align(
  center, 
  split(
    $E tack A arrow.squiggly #false quad E tack B arrow.squiggly #false$,
    $E tack A ==> B arrow.squiggly #true$,
  )
)

#align(
  center, 
  split(
    $E tack A arrow.squiggly #false quad E tack B arrow.squiggly #true$,
    $E tack A ==> B arrow.squiggly #false$,
  )
)

#align(
  center, 
  split(
    $E tack M arrow.squiggly m quad E tack N arrow.squiggly n$,
    $E tack M = N arrow.squiggly #true$,
    lateral: $("se" m = n)$ 
  )
)

#align(
  center, 
  split(
    $E tack M arrow.squiggly m quad E tack N arrow.squiggly n$,
    $E tack M = N arrow.squiggly #false$,
    lateral: $("se" m != n)$ 
  )
)

#align(
  center, 
  split(
    $E tack M arrow.squiggly m quad E tack N arrow.squiggly n$,
    $E tack M dot N arrow.squiggly v$,
    lateral: $("se" v = m dot n)$ 
  )
)

#align(
  center, 
  split(
    $E tack M arrow.squiggly m quad E tack N arrow.squiggly n$,
    $E tack M - N arrow.squiggly v$,
    lateral: $("se" v = m - n)$ 
  )
)

#align(
  center, 
  split(
    $E tack B arrow.squiggly #true quad E tack M arrow.squiggly m$,
    $E tack "if" B "then" M "else" N arrow.squiggly m$,
  )
)

#align(
  center, 
  split(
    $E tack B arrow.squiggly #false quad E tack N arrow.squiggly n$,
    $E tack "if" B "then" M "else" N arrow.squiggly n$,
  )
)

#align(
  center, 
  split(
    $E (x, (M, E)) tack N arrow.squiggly n$,
    $E tack "let" x = M "in" N arrow.squiggly n$,
  )
)

#align(
  center, 
  split(
    $E (f, (x, M, E)) tack N arrow.squiggly n$,
    $E tack "fun" f x = M "in" N arrow.squiggly n$,
  )
)

#align(
  center, 
  split(
    $E'(x, (N, E))(f, (x, M, E')) tack M arrow.squiggly m$,
    $E tack f N arrow.squiggly m$,
    lateral: $("se" E(f) = (x, M, E'))$
  )
)

#pagebreak()

// #align(
//   center, 
//   split(
//     $E(x, N, E') tack M' arrow.squiggly^v v$,
//     $E tack f N arrow.squiggly^v v$,
//     lateral: $("se" "Env"(f) = (x, M', E'))$,
//     top: 8pt
//   )
// )
//
// #align(
//   center,
//   split(
//     $E tack B arrow.squiggly^v "true" quad E tack M arrow.squiggly^v v $,
//     $E tack "if" B "then" M "else" N arrow.squiggly^v v$,
//     top: 8pt
//   )
// )
//
// #align(
//   center,
//   split(
//     $E tack B arrow.squiggly^v "false" quad E tack N arrow.squiggly^v v $,
//     $E tack "if" B "then" M "else" N arrow.squiggly^v v$,
//     top: 8pt
//   )
// )

// #align(
//   center, 
//   split(
//     
//     split(
//       split("over", "under", lateral: $("lateral")$), 
//       "come", 
//       lateral: $("stai"?)$
//     ),
//     $E tack x arrow.squiggly v$,
//     lateral: $("se" "Env"(x) = (M, E'))$
//   )
// )


// (context-free grammar)

// Hello darkness my old friend @cheat-sheet
// $ 
//   M, N ::= "integer" | "float" | "string" | 
// $
//
// == Parser
//
// - comments:
//   - "--" single line
//   - "{- -}" multiple line
//
// - keywords:
//   - case
//   - class
//   - data
//   - deriving
//   - do
//   - else
//   - if
//   - import
//   - in
//   - infix
//   - infixl
//   - infixr
//   - instance
//   - let
//   - of
//   - module
//   - newtype
//   - then
//   - type
//   - where
//
// - strings:
//   - "abc" unicode string (basically a list of chars)
//   - 'a' single character
//   - "multi \ line \ string" multiline string
//   
// - numbers:
//   - 1 integer
//   - 1.0 floating point
//
// - enumerations:
//   - [1..10] 1, ..., 10
//   - [100..] 100, 101, 102, ...
//   - [110..100] $emptyset$
//   - [0, -1 ..] negative integers?
//   - [-100..-110] syntax error, should be [-100.. -110]
//   - [1,3..100], [-1,3..100] list from 1 to 100 by 2,-1 to 100 by 4
//   - each value in the Enum class can be used?? What is a class?
//
// - lists & tuples:
//   - [] empty list
//   - [1,2,3] list of three numbers
//   - 1 : 2 : 3 : [] "cons"(:) and "nil"([])
//   - 'a' : 'b' : 'c' : [] same as "abc"
//   - (head, tail, 3, 'a', "abc") tuple of different elements
//
// - “Layout” rule, braces and semi-colons ??????
//   - basically python-like indentation for scopes, don't even think about ";" and "{}"
//
// - function definition
//   - square x = 
//       x \* x
//   - square x =
//         x2
//       where x2 = 
//         x \* x
//
// - let
//   - square x =
//       let x2 =
//         x \* x
//       in x2
//
// - case
//   - TODO:
//     - nesting, capture, matching order, guards
//
// - class
//   - TODO:
//     - class + instance
//     - overloading?
//     - defaults
//
// - data
//   - algebraic data types
//     - a.k.a. algebre induttive
//     - constructors with arguments
//     - type and constructor names
//     - type variables
//     - record syntax
//
// - deriving????
//
// - do
//   - monads??????????????????
//   - if and io
//
// - let
//   - deconstruction????????
//
// - of (riguarda le classi?)
//
// - module
//   - yay!
//   - imports???
//
// - data 
//   - creates a new type
//
// - type
//   - just aliases another type, they can be used interchangeably
//
// - newtype
//   - basycally create a new type, but behaves exactly like another type



= Monadi

#pagebreak()

#bibliography("bibliography.bib", title: "Bibliografia")

https://github.com/shwestrick/smlfmt
https://smlhelp.github.io/book/docs/
TODO: smlnj
TODO: millet

// https://hackage.haskell.org/package/CheatSheet-1.5/src/CheatSheet.pdf
