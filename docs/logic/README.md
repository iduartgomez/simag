# Agent Representation Language

Simag includes its own declarative logic programming language for working with
agent knowledge representation and the inference engine. this language is based
on first-order logic with a few important differences. The differences
with classic FOL are:

* Unique name assumption: each constant (term) refers to a single object or set.
* Includes a symbol for indicative conditional (and other for material
  implication).

  The indicative conditional is an operation in which if the antecedent
  is true, the consequent is true. Contrary to that, the material conditional 
  is always true, unless the consequent is false and the antecedent is true.
  
  The indicative conditional is used to assert rules for classification
  of objects and sets. In other words, is a more convenient way to
  assert rules about declarations.

  Indicative contional is equivalent to the "if ... then ..." control flow 
  of most programming languages.
* There is no negation. This is because every assertion must include
  a real valued truth statement which can go from 0 to 1. This follows from fuzzy
  logic and allow various degrees of membership to a set (for example), instead of binary membership (one either is part or isn't of a set). negation is achieved setting a membership to a class to 'zero'.